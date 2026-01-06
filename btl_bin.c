#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

#include "btl_chunk.h"
#include "btl_common.h"
#include "btl_memory.h"
#include "btl_object.h"
#include "btl_table.h"
#include "btl_value.h"
#include "btl_vm.h"

#define BTL_MAGIC 0x42544C21 // "BTL!"
#define BTL_VERSION 0

/* =============================================================================
 * 1. CONTEXTS AND CORE UTILITIES
 * ============================================================================= */

typedef struct {
    Table strings;        // Map: ObjString* -> int (ID)
    Table natives;        // Set of strings (native names)
    int poolCount;
} SerializationCtx;

typedef struct {
    ObjString** pool;     // ID -> ObjString*
    uint32_t poolCount;
} DeserializationCtx;

// LEB128 Unsigned Varint
static void writeVarint(FILE* f, uint32_t val) {
    uint8_t buf[5];
    int len = 0;
    do {
        uint8_t byte = val & 0x7F;
        val >>= 7;
        if (val != 0) byte |= 0x80;
        buf[len++] = byte;
    } while (val != 0);
    fwrite(buf, 1, len, f);
}

static uint32_t readVarint(FILE* f) {
    uint32_t res = 0, shift = 0;
    for (int i = 0; i < 5; i++) {
        int b = fgetc(f);
        if (b == EOF) exit(74);
        res |= (uint32_t)(b & 0x7F) << shift;
        if (!(b & 0x80)) return res;
        shift += 7;
    }
    return res;
}

// ZigZag Signed Varint
static void writeSignedVarint(FILE* f, int32_t val) {
    uint32_t zigzag = (val << 1) ^ (val >> 31);
    writeVarint(f, zigzag);
}

static int32_t readSignedVarint(FILE* f) {
    uint32_t zigzag = readVarint(f);
    return (int32_t)((zigzag >> 1) ^ -(int32_t)(zigzag & 1));
}

// Portable bit-consistent Double
static void writeDouble(FILE* f, double val) {
    union { double d; uint64_t u; } data;
    data.d = val;
    for (int i = 0; i < 8; i++) {
        fputc((uint8_t)((data.u >> (i * 8)) & 0xFF), f);
    }
}

static double readDouble(FILE* f) {
    union { double d; uint64_t u; } data;
    data.u = 0;
    for (int i = 0; i < 8; i++) {
        int b = fgetc(f);
        if (b == EOF) exit(74);
        data.u |= ((uint64_t)b << (i * 8));
    }
    return data.d;
}

/* =============================================================================
 * 2. PASS 1: DISCOVERY (STRINGS & NATIVES)
 * ============================================================================= */

static void discover(ObjFunction* fn, SerializationCtx* ctx) {
    // Collect function name
    if (fn->name != NULL) {
        if (!tableGet(&ctx->strings, fn->name, NULL)) {
            tableSet(&ctx->strings, fn->name, NUMBER_VAL(ctx->poolCount++));
        }
    }

    // Collect strings in constants and recurse into nested functions
    for (int i = 0; i < fn->chunk.constants.count; i++) {
        Value val = fn->chunk.constants.values[i];
        if (IS_STRING(val)) {
            if (!tableGet(&ctx->strings, AS_STRING(val), NULL)) {
                tableSet(&ctx->strings, AS_STRING(val), NUMBER_VAL(ctx->poolCount++));
            }
        } else if (IS_FUNCTION(val)) {
            discover(AS_FUNCTION(val), ctx);
        }
    }

    // Scan bytecode for Native references via Globals
    for (int i = 0; i < fn->chunk.count; i++) {
        uint8_t op = fn->chunk.code[i];
        if (op == OP_GET_GLOBAL || op == OP_SET_GLOBAL || op == OP_DEFINE_GLOBAL) {
            uint8_t arg = fn->chunk.code[i + 1];
            ObjString* name = AS_STRING(fn->chunk.constants.values[arg]);
            Value global;
            if (tableGet(&vm.globals, name, &global) && IS_NATIVE(global)) {
                tableSet(&ctx->natives, name, NIL_VAL);
            }
            i++; // skip operand
        }
    }
}

/* =============================================================================
 * 3. PASS 2: WRITING (SERIALIZATION)
 * ============================================================================= */

static void writeValue(FILE* f, Value val, SerializationCtx* ctx, bool stripped);

static void writeFunction(FILE* f, ObjFunction* fn, SerializationCtx* ctx, bool stripped) {
    writeVarint(f, fn->arity);
    writeVarint(f, fn->upvalueCount);

    if (stripped || fn->name == NULL) {
        writeVarint(f, 0xFFFFFFFF);
    } else {
        Value idx;
        tableGet(&ctx->strings, fn->name, &idx);
        writeVarint(f, (uint32_t)AS_NUMBER(idx));
    }

    // Code
    writeVarint(f, fn->chunk.count);
    fwrite(fn->chunk.code, 1, fn->chunk.count, f);

    // Compressed Line Deltas
    if (stripped) {
        writeVarint(f, 0);
    } else {
        int runs = 0;
        for (int i = 0; i < fn->chunk.count; i++) {
            while (i + 1 < fn->chunk.count && fn->chunk.lines[i] == fn->chunk.lines[i+1]) i++;
            runs++;
        }
        writeVarint(f, runs);
        int lastLine = 0;
        for (int i = 0; i < fn->chunk.count; i++) {
            int start = i;
            while (i + 1 < fn->chunk.count && fn->chunk.lines[i] == fn->chunk.lines[i+1]) i++;
            writeVarint(f, (i - start) + 1);
            writeSignedVarint(f, fn->chunk.lines[start] - lastLine);
            lastLine = fn->chunk.lines[start];
        }
    }

    // Constants
    writeVarint(f, fn->chunk.constants.count);
    for (int i = 0; i < fn->chunk.constants.count; i++) {
        writeValue(f, fn->chunk.constants.values[i], ctx, stripped);
    }
}

static void writeValue(FILE* f, Value val, SerializationCtx* ctx, bool stripped) {
    if (IS_NIL(val)) { fputc('n', f); }
    else if (IS_BOOL(val)) { fputc(AS_BOOL(val) ? 't' : 'f', f); }
    else if (IS_NUMBER(val)) {
        double d = AS_NUMBER(val);
        if (d == (double)(uint8_t)d) { fputc('1', f); fputc((uint8_t)d, f); }
        else { fputc('d', f); writeDouble(f, d); }
    } else if (IS_STRING(val)) {
        fputc('S', f);
        Value idx;
        tableGet(&ctx->strings, AS_STRING(val), &idx);
        writeVarint(f, (uint32_t)AS_NUMBER(idx));
    } else if (IS_FUNCTION(val)) {
        fputc('F', f);
        writeFunction(f, AS_FUNCTION(val), ctx, stripped);
    }
}

/* =============================================================================
 * 4. READING (DESERIALIZATION)
 * ============================================================================= */

static Value readValue(FILE* f, DeserializationCtx* ctx);

static ObjFunction* readFunction(FILE* f, DeserializationCtx* ctx) {
    ObjFunction* fn = newFunction();
    push(OBJ_VAL(fn)); // GC Guard

    fn->arity = readVarint(f);
    fn->upvalueCount = readVarint(f);

    uint32_t nameIdx = readVarint(f);
    if (nameIdx != 0xFFFFFFFF) fn->name = ctx->pool[nameIdx];

    fn->chunk.count = fn->chunk.capacity = readVarint(f);
    fn->chunk.code = ALLOCATE(uint8_t, fn->chunk.count);
    fread(fn->chunk.code, 1, fn->chunk.count, f);

    // Line Deltas
    int runs = readVarint(f);
    if (runs > 0) {
        fn->chunk.lines = ALLOCATE(int, fn->chunk.count);
        int cur = 0, lastLine = 0;
        for (int i = 0; i < runs; i++) {
            int len = readVarint(f);
            int delta = readSignedVarint(f);
            lastLine += delta;
            for (int j = 0; j < len; j++) if (cur < fn->chunk.count) fn->chunk.lines[cur++] = lastLine;
        }
    }

    // Constants
    int constants = readVarint(f);
    for (int i = 0; i < constants; i++) {
        writeValueArray(&fn->chunk.constants, readValue(f, ctx));
    }

    pop(); // Remove GC Guard
    return fn;
}

static Value readValue(FILE* f, DeserializationCtx* ctx) {
    int tag = fgetc(f);
    switch (tag) {
        case 'n': return NIL_VAL;
        case 't': return BOOL_VAL(true);
        case 'f': return BOOL_VAL(false);
        case '1': return NUMBER_VAL((double)fgetc(f));
        case 'd': return NUMBER_VAL(readDouble(f));
        case 'S': return OBJ_VAL(ctx->pool[readVarint(f)]);
        case 'F': return OBJ_VAL(readFunction(f, ctx));
        default:  return NIL_VAL;
    }
}

/* =============================================================================
 * 5. PUBLIC API
 * ============================================================================= */

void saveBtlBinary(ObjFunction* script, const char* path, bool stripped) {
    SerializationCtx ctx;
    initTable(&ctx.strings);
    initTable(&ctx.natives);
    ctx.poolCount = 0;

    discover(script, &ctx);

    FILE* f = fopen(path, "wb");
    if (!f) { freeTable(&ctx.strings); freeTable(&ctx.natives); return; }

    // Header
    uint32_t magic = BTL_MAGIC;
    fwrite(&magic, 4, 1, f);
    fputc(BTL_VERSION, f);
    fputc(stripped ? 1 : 0, f);

    // Native Manifest
    writeVarint(f, ctx.natives.count);
    for (int i = 0; i < ctx.natives.capacity; i++) {
        Entry* entry = &ctx.natives.entries[i];
        if (entry->key == NULL) continue;
        writeVarint(f, entry->key->length);
        fwrite(entry->key->chars, 1, entry->key->length, f);
    }

    // Global String Pool
    writeVarint(f, ctx.poolCount);
    ObjString** sortedPool = malloc(sizeof(ObjString*) * ctx.poolCount);
    for (int i = 0; i < ctx.strings.capacity; i++) {
        Entry* e = &ctx.strings.entries[i];
        if (e->key != NULL) sortedPool[(int)AS_NUMBER(e->value)] = e->key;
    }
    for (int i = 0; i < ctx.poolCount; i++) {
        writeVarint(f, sortedPool[i]->length);
        fwrite(sortedPool[i]->chars, 1, sortedPool[i]->length, f);
    }

    writeFunction(f, script, &ctx, stripped);

    fclose(f);
    free(sortedPool);
    freeTable(&ctx.strings);
    freeTable(&ctx.natives);
}

ObjFunction* loadBtlBinary(const char* path) {
    FILE* f = fopen(path, "rb");
    if (!f) return NULL;

    uint32_t magic;
    fread(&magic, 4, 1, f);
    if (magic != BTL_MAGIC || fgetc(f) != BTL_VERSION) { fclose(f); return NULL; }
    fgetc(f); // flags

    // Verify Natives
    int nativeCount = readVarint(f);
    for (int i = 0; i < nativeCount; i++) {
        int len = readVarint(f);
        char* name = malloc(len + 1);
        fread(name, 1, len, f); name[len] = '\0';
        Value native;
        if (!tableGet(&vm.globals, copyString(name, len), &native)) {
            fprintf(stderr, "Requirement Missing: %s\n", name);
            exit(74);
        }
        free(name);
    }

    // Load String Pool (GC Safety)
    DeserializationCtx ctx;
    ctx.poolCount = readVarint(f);
    ctx.pool = ALLOCATE(ObjString*, ctx.poolCount);
    for (uint32_t i = 0; i < ctx.poolCount; i++) {
        int len = readVarint(f);
        char* chars = malloc(len + 1);
        fread(chars, 1, len, f); chars[len] = '\0';
        ctx.pool[i] = copyString(chars, len);
        free(chars);
        push(OBJ_VAL(ctx.pool[i])); // Stack anchor for GC
    }

    ObjFunction* script = readFunction(f, &ctx);

    for (uint32_t i = 0; i < ctx.poolCount; i++) pop();
    FREE_ARRAY(ObjString*, ctx.pool, ctx.poolCount);
    fclose(f);
    return script;
}
