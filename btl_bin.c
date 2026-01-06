#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

/* Standard BTL Headers */
#include "btl_chunk.h"
#include "btl_common.h"
#include "btl_memory.h"
#include "btl_object.h"
#include "btl_table.h"
#include "btl_value.h"
#include "btl_vm.h"

/* File Identification: "BTL!" and Version */
#define BTL_MAGIC_0 0x42 
#define BTL_MAGIC_1 0x54 
#define BTL_MAGIC_2 0x4C 
#define BTL_MAGIC_3 0x21 
#define BTL_VERSION 0

/* =============================================================================
 * 1. CONTEXTS AND CORE UTILITIES
 * ============================================================================= */

/* Used during the writing phase to collect all unique assets in the script. */
typedef struct {
    Table strings;        // Hash table to deduplicate strings: ObjString* -> int ID
    Table natives;        // Set of strings: Names of native C functions required
    int poolCount;        // Unique string counter
} SerializationCtx;

/* Used during the loading phase to map IDs back to live ObjString pointers. */
typedef struct {
    ObjString** pool;     // Array of pointers: ID -> live ObjString*
    uint32_t poolCount;   // Size of the pool
} DeserializationCtx;

/**
 * LEB128 (Little-Endian Base 128) Variable-Length Integer Encoding.
 * Most numbers in bytecode (arity, counts, indices) are small. Instead of 
 * wasting 4 bytes for the number '2', this uses only 1 byte.
 * The 8th bit of each byte is a "continuation" flag.
 */
static void writeVarint(FILE* f, uint32_t val) {
    uint8_t buf[5];
    int len = 0;
    do {
        uint8_t byte = val & 0x7F;
        val >>= 7;
        if (val != 0) byte |= 0x80; // Set high bit if more bytes follow
        buf[len++] = byte;
    } while (val != 0);
    fwrite(buf, 1, len, f);
}

static uint32_t readVarint(FILE* f) {
    uint32_t res = 0, shift = 0;
    for (int i = 0; i < 5; i++) {
        int b = fgetc(f);
        if (b == EOF) {
            fprintf(stderr, "BTL Error: Truncated Varint.\n");
            exit(74);
        }
        res |= (uint32_t)(b & 0x7F) << shift;
        if (!(b & 0x80)) return res; // High bit not set: this is the last byte
        shift += 7;
    }
    return res;
}

/**
 * ZigZag Encoding for Signed Integers.
 * LEB128 is inefficient for negative numbers. ZigZag maps signed numbers 
 * onto unsigned ones (0->0, -1->1, 1->2, -2->3) so small negatives 
 * compress just as well as small positives. Used for line deltas.
 */
static void writeSignedVarint(FILE* f, int32_t val) {
    uint32_t zigzag = (val << 1) ^ (val >> 31);
    writeVarint(f, zigzag);
}

static int32_t readSignedVarint(FILE* f) {
    uint32_t zigzag = readVarint(f);
    return (int32_t)((zigzag >> 1) ^ -(int32_t)(zigzag & 1));
}

/**
 * Portable Double I/O.
 * Directly writing a double depends on the CPU's Endianness. This breaks
 * the 64-bit float into 8 fixed bytes, ensuring a file saved on Intel 
 * works on ARM or PowerPC.
 */
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

/* Returns the total bytes an instruction occupies, allowing the scanner to 
   jump through bytecode without losing synchronization. */
static int btlInstructionLength(uint8_t* code, int offset) {
    switch (code[offset]) {
        case OP_CONSTANT:    case OP_CLASS:        case OP_GET_SUPER:
        case OP_DEFINE_GLOBAL: case OP_GET_GLOBAL: case OP_SET_GLOBAL:
        case OP_GET_LOCAL:   case OP_SET_LOCAL:    case OP_GET_UPVALUE:
        case OP_SET_UPVALUE: case OP_GET_PROPERTY: case OP_SET_PROPERTY:
        case OP_CALL:        case OP_METHOD:       return 2;
        case OP_JUMP:        case OP_JUMP_IF_FALSE: case OP_LOOP:
        case OP_INVOKE:      case OP_SUPER_INVOKE: return 3;
        default:             return 1;
    }
}

/**
 * Recursive Discovery Pass.
 * Scans the function tree to:
 * 1. Find every unique string (names, constants) to build a Global String Pool.
 * 2. Identify required native functions to build the Native Manifest.
 */
static void discover(ObjFunction* fn, SerializationCtx* ctx) {
    if (fn->name != NULL) {
        if (!tableGet(&ctx->strings, fn->name, NULL)) {
            tableSet(&ctx->strings, fn->name, NUMBER_VAL(ctx->poolCount++));
        }
    }

    // Traverse Constant Pool
    for (int i = 0; i < fn->chunk.constants.count; i++) {
        Value val = fn->chunk.constants.values[i];
        if (IS_STRING(val)) {
            if (!tableGet(&ctx->strings, AS_STRING(val), NULL)) {
                tableSet(&ctx->strings, AS_STRING(val), NUMBER_VAL(ctx->poolCount++));
            }
        } else if (IS_FUNCTION(val)) {
            discover(AS_FUNCTION(val), ctx); // Recurse into nested functions
        }
    }

    // Scan Bytecode for Global/Native usage
    for (int i = 0; i < fn->chunk.count; ) {
        uint8_t op = fn->chunk.code[i];
        if (op == OP_GET_GLOBAL || op == OP_SET_GLOBAL || op == OP_DEFINE_GLOBAL) {
            uint8_t arg = fn->chunk.code[i + 1];
            if (arg < fn->chunk.constants.count) {
                Value nameVal = fn->chunk.constants.values[arg];
                if (IS_STRING(nameVal)) {
                    ObjString* name = AS_STRING(nameVal);
                    Value global;
                    // Check if this global name is currently a native in the VM
                    if (tableGet(&vm.globals, name, &global) && IS_NATIVE(global)) {
                        tableSet(&ctx->natives, name, NIL_VAL);
                    }
                }
            }
        }
        
        // Closures have a variable length based on upvalue count
        if (op == OP_CLOSURE) {
            uint8_t constant = fn->chunk.code[i + 1];
            if (constant < fn->chunk.constants.count) {
                Value funcVal = fn->chunk.constants.values[constant];
                if (IS_FUNCTION(funcVal)) {
                    ObjFunction* loadedFn = AS_FUNCTION(funcVal);
                    i += 2 + (loadedFn->upvalueCount * 2);
                    continue;
                }
            }
            i += 2;
        } else {
            i += btlInstructionLength(fn->chunk.code, i);
        }
    }
}

/* =============================================================================
 * 3. PASS 2: WRITING (SERIALIZATION)
 * ============================================================================= */

static void writeValue(FILE* f, Value val, SerializationCtx* ctx, bool stripped);

/**
 * Serializes an ObjFunction, its bytecode, and its constant pool.
 * Uses Delta Compression for line numbers (storing only differences).
 */
static void writeFunction(FILE* f, ObjFunction* fn, SerializationCtx* ctx, bool stripped) {
    writeVarint(f, fn->arity);
    writeVarint(f, fn->upvalueCount);

    // Write name as an ID from the String Pool
    if (stripped || fn->name == NULL) {
        writeVarint(f, 0xFFFFFFFF);
    } else {
        Value idx;
        tableGet(&ctx->strings, fn->name, &idx);
        writeVarint(f, (uint32_t)AS_NUMBER(idx));
    }

    // Raw Bytecode
    writeVarint(f, fn->chunk.count);
    fwrite(fn->chunk.code, 1, fn->chunk.count, f);

    // Line Data (Run-Length Encoding + ZigZag Deltas)
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
            writeVarint(f, (i - start) + 1); // Length of run
            writeSignedVarint(f, fn->chunk.lines[start] - lastLine); // Delta
            lastLine = fn->chunk.lines[start];
        }
    }

    // Constant Pool
    writeVarint(f, fn->chunk.constants.count);
    for (int i = 0; i < fn->chunk.constants.count; i++) {
        writeValue(f, fn->chunk.constants.values[i], ctx, stripped);
    }
}

/* Serializes a Btl Value using one-byte type tags. */
static void writeValue(FILE* f, Value val, SerializationCtx* ctx, bool stripped) {
    if (IS_NIL(val)) { fputc('n', f); }
    else if (IS_BOOL(val)) { fputc(AS_BOOL(val) ? 't' : 'f', f); }
    else if (IS_NUMBER(val)) {
        double d = AS_NUMBER(val);
        // Optimization: if it's a small integer, store in 1 byte
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
    push(OBJ_VAL(fn)); // GC Safety: Anchor function so load triggers don't reap it

    fn->arity = readVarint(f);
    fn->upvalueCount = readVarint(f);

    uint32_t nameIdx = readVarint(f);
    if (nameIdx != 0xFFFFFFFF) {
        if (nameIdx >= ctx->poolCount) exit(74);
        fn->name = ctx->pool[nameIdx];
    }

    fn->chunk.count = fn->chunk.capacity = readVarint(f);
    fn->chunk.code = ALLOCATE(uint8_t, fn->chunk.count);
    if (fread(fn->chunk.code, 1, fn->chunk.count, f) < (size_t)fn->chunk.count) exit(74);

    // Decompress Line Info
    int runs = readVarint(f);
    if (runs > 0) {
        fn->chunk.lines = ALLOCATE(int, fn->chunk.count);
        int cur = 0, lastLine = 0;
        for (int i = 0; i < runs; i++) {
            int len = readVarint(f);
            int delta = readSignedVarint(f);
            lastLine += delta;
            for (int j = 0; j < len; j++) {
                if (cur < fn->chunk.count) fn->chunk.lines[cur++] = lastLine;
            }
        }
    }

    // Constants
    int constants = readVarint(f);
    for (int i = 0; i < constants; i++) {
        writeValueArray(&fn->chunk.constants, readValue(f, ctx));
    }

    pop(); // Remove GC Anchor
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
        case 'S': {
            uint32_t idx = readVarint(f);
            if (idx >= ctx->poolCount) exit(74);
            return OBJ_VAL(ctx->pool[idx]);
        }
        case 'F': return OBJ_VAL(readFunction(f, ctx));
        default:  return NIL_VAL;
    }
}

/* =============================================================================
 * 5. PUBLIC API
 * ============================================================================= */

/**
 * Saves a compiled  script to a BTL binary file.
 * Set 'stripped' to true to remove debug info (line numbers/names).
 */
void saveBtlBinary(ObjFunction* script, const char* path, bool stripped) {
    push(OBJ_VAL(script)); // GC Anchor for the root object
    
    SerializationCtx ctx;
    initTable(&ctx.strings);
    initTable(&ctx.natives);
    ctx.poolCount = 0;
    
    discover(script, &ctx); // Pass 1

    FILE* f = fopen(path, "wb");
    if (!f) { 
        freeTable(&ctx.strings); freeTable(&ctx.natives); pop(); return; 
    }

    // 1. Header
    fputc(BTL_MAGIC_0, f); fputc(BTL_MAGIC_1, f);
    fputc(BTL_MAGIC_2, f); fputc(BTL_MAGIC_3, f);
    fputc(BTL_VERSION, f);
    fputc(stripped ? 1 : 0, f);

    // 2. Native Manifest
    writeVarint(f, ctx.natives.count);
    for (int i = 0; i < ctx.natives.capacity; i++) {
        Entry* entry = &ctx.natives.entries[i];
        if (entry->key == NULL) continue;
        writeVarint(f, entry->key->length);
        fwrite(entry->key->chars, 1, entry->key->length, f);
    }

    // 3. String Pool (Sorted by ID)
    writeVarint(f, ctx.poolCount);
    ObjString** sortedPool = NULL;
    if (ctx.poolCount > 0) {
        sortedPool = (ObjString**)malloc(sizeof(ObjString*) * ctx.poolCount);
        if (!sortedPool) exit(1);
        for (int i = 0; i < ctx.strings.capacity; i++) {
            Entry* e = &ctx.strings.entries[i];
            if (e->key != NULL) sortedPool[(int)AS_NUMBER(e->value)] = e->key;
        }
        for (int i = 0; i < ctx.poolCount; i++) {
            writeVarint(f, sortedPool[i]->length);
            fwrite(sortedPool[i]->chars, 1, sortedPool[i]->length, f);
        }
    }

    // 4. Recursive Code Dump
    writeFunction(f, script, &ctx, stripped); // Pass 2
    
    fclose(f);
    if (sortedPool) free(sortedPool);
    freeTable(&ctx.strings); freeTable(&ctx.natives);
    pop(); 
}

/**
 * Loads a BTL binary into the VM.
 * Verifies magic number, version, and requirements before execution.
 */
ObjFunction* loadBtlBinary(const char* path) {
    FILE* f = fopen(path, "rb");
    if (!f) return NULL;

    // Verify Header
    if (fgetc(f) != BTL_MAGIC_0 || fgetc(f) != BTL_MAGIC_1 ||
        fgetc(f) != BTL_MAGIC_2 || fgetc(f) != BTL_MAGIC_3) {
        fclose(f); return NULL;
    }
    if (fgetc(f) != BTL_VERSION) { fclose(f); return NULL; }
    fgetc(f); // skip flags

    // 1. Requirement Validation (Natives)
    int nativeCount = readVarint(f);
    for (int i = 0; i < nativeCount; i++) {
        int len = readVarint(f);
        char* name = malloc(len + 1);
        if (fread(name, 1, len, f) < (size_t)len) { free(name); fclose(f); exit(74); }
        name[len] = '\0';
        
        ObjString* nameStr = copyString(name, len);
        push(OBJ_VAL(nameStr)); // GC Anchor
        Value native;
        bool found = tableGet(&vm.globals, nameStr, &native);
        pop(); // nameStr
        
        if (!found || !IS_NATIVE(native)) {
            fprintf(stderr, "BTL Error: Required native '%s' not found in this VM.\n", name);
            free(name); fclose(f); exit(74);
        }
        free(name);
    }

    // 2. Load String Pool
    DeserializationCtx ctx;
    ctx.poolCount = readVarint(f);
    ctx.pool = NULL;
    if (ctx.poolCount > 0) {
        // Safety: ensure pool doesn't exceed btl's fixed stack limits during anchoring
        if (ctx.poolCount > 250) {
            fprintf(stderr, "BTL Error: String pool too large.\n");
            fclose(f); exit(74);
        }

        ctx.pool = ALLOCATE(ObjString*, ctx.poolCount);
        for (uint32_t i = 0; i < ctx.poolCount; i++) {
            int len = readVarint(f);
            char* chars = malloc(len + 1);
            if (fread(chars, 1, len, f) < (size_t)len) { free(chars); fclose(f); exit(74); }
            chars[len] = '\0';
            ctx.pool[i] = copyString(chars, len);
            free(chars);
            
            // GC Safety: Anchoring strings as they are interned. Strings in btl
            // are weakly referenced in the string table, so they must be on stack.
            push(OBJ_VAL(ctx.pool[i])); 
        }
    }

    // 3. Reconstruct Script Object
    ObjFunction* script = readFunction(f, &ctx);

    // Cleanup anchors
    if (ctx.poolCount > 0) {
        for (uint32_t i = 0; i < ctx.poolCount; i++) pop();
        FREE_ARRAY(ObjString*, ctx.pool, ctx.poolCount);
    }
    
    fclose(f);
    return script;
}
