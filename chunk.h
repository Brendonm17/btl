#ifndef btl_chunk_h
#define btl_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    // ========================================================================
    // CONSTANTS & LITERALS
    // ========================================================================
    OP_CONSTANT,            // [idx:8]        | Push constants[idx]
    OP_CONSTANT_LONG,       // [idx:16]       | Push constants[idx]
    OP_NIL,                 //                | Push nil
    OP_TRUE,                //                | Push true
    OP_FALSE,               //                | Push false
    OP_0,                   //                | Push 0.0
    OP_1,                   //                | Push 1.0
    OP_2,                   //                | Push 2.0

    // ========================================================================
    // STACK MANIPULATION
    // ========================================================================
    OP_POP,                 //                | Pop top value
    OP_POP_N,               // [n:8]          | Pop n values
    OP_DUP,                 //                | Duplicate top value
    OP_SWAP,                //                | Swap top two values

    // ========================================================================
    // LOCAL VARIABLES
    // ========================================================================
    OP_GET_LOCAL,           // [slot:8]       | Push locals[slot]
    OP_GET_LOCAL_0,         //                | Push locals[0]
    OP_GET_LOCAL_1,         //                | Push locals[1]
    OP_GET_LOCAL_2,         //                | Push locals[2]
    OP_GET_LOCAL_3,         //                | Push locals[3]
    OP_GET_LOCAL_4,         //                | Push locals[4]
    OP_GET_LOCAL_5,         //                | Push locals[5]
    OP_GET_LOCAL_6,         //                | Push locals[6]
    OP_GET_LOCAL_7,         //                | Push locals[7]
    OP_SET_LOCAL,           // [slot:8]       | locals[slot] = peek(0)
    OP_SET_LOCAL_0,         //                | locals[0] = peek(0)
    OP_SET_LOCAL_1,         //                | locals[1] = peek(0)
    OP_SET_LOCAL_2,         //                | locals[2] = peek(0)
    OP_SET_LOCAL_3,         //                | locals[3] = peek(0)
    OP_SET_LOCAL_4,         //                | locals[4] = peek(0)
    OP_SET_LOCAL_5,         //                | locals[5] = peek(0)
    OP_SET_LOCAL_6,         //                | locals[6] = peek(0)
    OP_SET_LOCAL_7,         //                | locals[7] = peek(0)
    OP_SET_LOCAL_0_POP,     //                | locals[0] = pop()
    OP_SET_LOCAL_1_POP,     //                | locals[1] = pop()
    OP_SET_LOCAL_2_POP,     //                | locals[2] = pop()
    OP_SET_LOCAL_3_POP,     //                | locals[3] = pop()
    OP_SET_LOCAL_4_POP,     //                | locals[4] = pop()
    OP_SET_LOCAL_5_POP,     //                | locals[5] = pop()
    OP_SET_LOCAL_6_POP,     //                | locals[6] = pop()
    OP_SET_LOCAL_7_POP,     //                | locals[7] = pop()

    // ========================================================================
    // INCREMENT / DECREMENT
    // ========================================================================
    OP_INC_LOCAL_POP,       // [slot:8]       | locals[slot]++
    OP_INC_LOCAL,           // [slot:8]       | Push ++locals[slot]
    OP_INCREMENT,           //                | Push pop() + 1
    OP_DECREMENT,           //                | Push pop() - 1

    // ========================================================================
    // GLOBAL VARIABLES
    // ========================================================================
    OP_GET_GLOBAL,          // [idx:8]        | Push globals[idx]
    OP_GET_GLOBAL_LONG,     // [idx:16]       | Push globals[idx]
    OP_DEFINE_GLOBAL,       // [idx:8]        | globals[idx] = pop()
    OP_DEFINE_GLOBAL_LONG,  // [idx:16]       | globals[idx] = pop()
    OP_SET_GLOBAL,          // [idx:8]        | globals[idx] = peek(0)
    OP_SET_GLOBAL_LONG,     // [idx:16]       | globals[idx] = peek(0)

    // ========================================================================
    // UPVALUES (Closures)
    // ========================================================================
    // Generic upvalue access (self-patching to specific variant)
    OP_GET_UPVALUE,         // [slot:8]       | Push upvalue[slot]
    OP_GET_UPVALUE_OPEN,    // [slot:8]       | Push *upvalue[slot].stack
    OP_GET_UPVALUE_CLOSED,  // [slot:8]       | Push upvalue[slot].box->closed
    OP_GET_UPVALUE_IMMUTABLE,// [slot:8]      | Push upvalue[slot].immValue
    OP_SET_UPVALUE,         // [slot:8]       | upvalue[slot] = peek(0)
    OP_SET_UPVALUE_OPEN,    // [slot:8]       | *upvalue[slot].stack = peek(0)
    OP_SET_UPVALUE_CLOSED,  // [slot:8]       | upvalue[slot].box->closed = peek(0)

    // Specialized upvalue opcodes for slots 0-3 (no operand byte)
    OP_GET_UPVALUE_0,       //                | Push upvalue[0]
    OP_GET_UPVALUE_OPEN_0,  //                | Push *upvalue[0].stack
    OP_GET_UPVALUE_CLOSED_0,//                | Push upvalue[0].box->closed
    OP_GET_UPVALUE_IMMUTABLE_0,//             | Push upvalue[0].immValue
    OP_SET_UPVALUE_0,       //                | upvalue[0] = peek(0)
    OP_SET_UPVALUE_OPEN_0,  //                | *upvalue[0].stack = peek(0)
    OP_SET_UPVALUE_CLOSED_0,//                | upvalue[0].box->closed = peek(0)
    OP_GET_UPVALUE_1,
    OP_GET_UPVALUE_OPEN_1,
    OP_GET_UPVALUE_CLOSED_1,
    OP_GET_UPVALUE_IMMUTABLE_1,
    OP_SET_UPVALUE_1,
    OP_SET_UPVALUE_OPEN_1,
    OP_SET_UPVALUE_CLOSED_1,
    OP_GET_UPVALUE_2,
    OP_GET_UPVALUE_OPEN_2,
    OP_GET_UPVALUE_CLOSED_2,
    OP_GET_UPVALUE_IMMUTABLE_2,
    OP_SET_UPVALUE_2,
    OP_SET_UPVALUE_OPEN_2,
    OP_SET_UPVALUE_CLOSED_2,
    OP_GET_UPVALUE_3,
    OP_GET_UPVALUE_OPEN_3,
    OP_GET_UPVALUE_CLOSED_3,
    OP_GET_UPVALUE_IMMUTABLE_3,
    OP_SET_UPVALUE_3,
    OP_SET_UPVALUE_OPEN_3,
    OP_SET_UPVALUE_CLOSED_3,

    // ========================================================================
    // FIELDS & PROPERTIES
    // ========================================================================
    OP_FIELD,               // [name_idx:8]   | Register field name on class
    OP_GET_FIELD_THIS,      // [idx:8]        | Push this.fields[idx]
    OP_SET_FIELD_THIS,      // [idx:8]        | this.fields[idx] = peek(0)
    OP_GET_PROPERTY_IC,     // [name:8][ic:8] | Push receiver.property (inline cached)
    OP_SET_PROPERTY_IC,     // [name:8][ic:8] | receiver.property = peek(0) (inline cached)
    OP_GET_SUPER,           // [name:8]       | Push bound method from superclass
    OP_GET_SUPER_LONG,      // [name:16]      | Push bound method from superclass

    // ========================================================================
    // COMPARISON & ARITHMETIC
    // ========================================================================
    OP_EQUAL,               //                | Push pop() == pop()
    OP_GREATER,             //                | Push pop() > pop()  (b > a)
    OP_LESS,                //                | Push pop() < pop()  (b < a)
    OP_ADD,                 //                | Push pop() + pop()
    OP_SUBTRACT,            //                | Push b - a (a=pop, b=pop)
    OP_MULTIPLY,            //                | Push pop() * pop()
    OP_DIVIDE,              //                | Push b / a
    OP_MODULO,              //                | Push b % a
    OP_NOT,                 //                | Push !pop()
    OP_NEGATE,              //                | Push -pop()

    // ========================================================================
    // I/O
    // ========================================================================
    OP_PRINT,               //                | Print pop() to stdout

    // ========================================================================
    // CONTROL FLOW
    // ========================================================================
    OP_JUMP,                // [offset:16]    | ip += offset
    OP_JUMP_IF_FALSE,       // [offset:16]    | if falsey(peek(0)) ip += offset
    OP_POP_JUMP_IF_FALSE,   // [offset:16]    | if falsey(pop()) ip += offset
    OP_JUMP_IF_TRUE,        // [offset:16]    | if truthy(peek(0)) ip += offset
    OP_POP_JUMP_IF_TRUE,    // [offset:16]    | if truthy(pop()) ip += offset
    OP_JUMP_IF_NOT_EQUAL,   // [offset:16]    | if pop() != pop() ip += offset
    OP_JUMP_IF_EQUAL,       // [offset:16]    | if pop() == pop() ip += offset
    OP_JUMP_IF_NOT_GREATER, // [offset:16]    | if !(b > a) ip += offset
    OP_JUMP_IF_NOT_LESS,    // [offset:16]    | if !(b < a) ip += offset
    OP_LOOP,                // [offset:16]    | ip -= offset

    // ========================================================================
    // FUNCTION CALLS
    // ========================================================================
    // Regular calls: push result
    OP_CALL_0,              //                | Call peek(0) with 0 args
    OP_CALL_1,              //                | Call peek(1) with 1 arg
    OP_CALL_2,              //                | Call peek(2) with 2 args
    OP_CALL_3,              //                | Call peek(3) with 3 args
    OP_CALL_4,              //                | Call peek(4) with 4 args
    OP_CALL_5,              //                | Call peek(5) with 5 args
    OP_CALL_6,              //                | Call peek(6) with 6 args
    OP_CALL_7,              //                | Call peek(7) with 7 args
    OP_CALL_8,              //                | Call peek(8) with 8 args
    OP_CALL,                // [argc:8]       | Call peek(argc) with argc args

    // Tail calls: reuse current frame
    OP_TAIL_CALL_0,
    OP_TAIL_CALL_1,
    OP_TAIL_CALL_2,
    OP_TAIL_CALL_3,
    OP_TAIL_CALL_4,
    OP_TAIL_CALL_5,
    OP_TAIL_CALL_6,
    OP_TAIL_CALL_7,
    OP_TAIL_CALL_8,
    OP_TAIL_CALL,           // [argc:8]       | Tail call with argc args

    // ========================================================================
    // METHOD INVOCATION (Indexed - compile-time resolved)
    // ========================================================================
    // Stack: [receiver, arg0, arg1, ...] -> [result]
    OP_INVOKE_0,            // [idx:8]        | Call receiver.methods[idx] with 0 args
    OP_INVOKE_1,            // [idx:8]        | Call receiver.methods[idx] with 1 arg
    OP_INVOKE_2,            // [idx:8]        | Call receiver.methods[idx] with 2 args
    OP_INVOKE_3,            // [idx:8]        | Call receiver.methods[idx] with 3 args
    OP_INVOKE_4,            // [idx:8]        | Call receiver.methods[idx] with 4 args
    OP_INVOKE_5,            // [idx:8]        | Call receiver.methods[idx] with 5 args
    OP_INVOKE_6,            // [idx:8]        | Call receiver.methods[idx] with 6 args
    OP_INVOKE_7,            // [idx:8]        | Call receiver.methods[idx] with 7 args
    OP_INVOKE_8,            // [idx:8]        | Call receiver.methods[idx] with 8 args

    // Tail invoke (indexed)
    OP_TAIL_INVOKE_0,       // [idx:8]        | Tail call methods[idx] with 0 args
    OP_TAIL_INVOKE_1,
    OP_TAIL_INVOKE_2,
    OP_TAIL_INVOKE_3,
    OP_TAIL_INVOKE_4,
    OP_TAIL_INVOKE_5,
    OP_TAIL_INVOKE_6,
    OP_TAIL_INVOKE_7,
    OP_TAIL_INVOKE_8,

    // Invoke with variable arg count (indexed)
    OP_INVOKE,              // [idx:8][argc:8]      | Call methods[idx] with argc args
    OP_INVOKE_LONG,         // [idx:16][argc:8]     | Call methods[idx] with argc args

    // ========================================================================
    // METHOD INVOCATION (IC - runtime name lookup with caching)
    // ========================================================================
    OP_INVOKE_IC,           // [name:8][argc:8][ic:8] | Call by name, cache result

    // Tail invoke with variable arg count (indexed)
    OP_TAIL_INVOKE,         // [idx:8][argc:8]      | Tail call methods[idx]
    OP_TAIL_INVOKE_LONG,    // [idx:16][argc:8]     | Tail call methods[idx]

    // Tail invoke IC
    OP_TAIL_INVOKE_IC,      // [name:8][argc:8][ic:8] | Tail call by name, cache result

    // ========================================================================
    // SUPER METHOD INVOCATION (Always indexed - compile-time resolved)
    // ========================================================================
    // Stack: [this, super, arg0, arg1, ...] -> [result]
    OP_SUPER_INVOKE_0,      // [idx:8]        | Call super.methods[idx] with 0 args
    OP_SUPER_INVOKE_1,
    OP_SUPER_INVOKE_2,
    OP_SUPER_INVOKE_3,
    OP_SUPER_INVOKE_4,
    OP_SUPER_INVOKE_5,
    OP_SUPER_INVOKE_6,
    OP_SUPER_INVOKE_7,
    OP_SUPER_INVOKE_8,

    // Tail super invoke
    OP_TAIL_SUPER_INVOKE_0, // [idx:8]        | Tail call super.methods[idx]
    OP_TAIL_SUPER_INVOKE_1,
    OP_TAIL_SUPER_INVOKE_2,
    OP_TAIL_SUPER_INVOKE_3,
    OP_TAIL_SUPER_INVOKE_4,
    OP_TAIL_SUPER_INVOKE_5,
    OP_TAIL_SUPER_INVOKE_6,
    OP_TAIL_SUPER_INVOKE_7,
    OP_TAIL_SUPER_INVOKE_8,

    // Super invoke with variable arg count
    OP_SUPER_INVOKE,        // [idx:8][argc:8]      | Call super.methods[idx]
    OP_SUPER_INVOKE_LONG,   // [idx:16][argc:8]     | Call super.methods[idx]
    OP_TAIL_SUPER_INVOKE,   // [idx:8][argc:8]      | Tail call super.methods[idx]
    OP_TAIL_SUPER_INVOKE_LONG,// [idx:16][argc:8]   | Tail call super.methods[idx]

    // ========================================================================
    // CLOSURES
    // ========================================================================
    OP_CLOSURE,             // [fn:8] [upvals...] | Create closure from constants[fn]
    OP_CLOSURE_LONG,        // [fn:16][upvals...] | Create closure from constants[fn]
    OP_CLOSE_UPVALUE,       //                    | Close upvalues for top stack slot, pop

    // ========================================================================
    // RETURN
    // ========================================================================
    OP_RETURN,              //                | Return pop() from current function

    // ========================================================================
    // CLASSES
    // ========================================================================
    OP_CLASS,               // [name:8]       | Create class, push it
    OP_CLASS_LONG,          // [name:16]      | Create class, push it
    OP_INHERIT,             //                | Pop subclass, pop superclass, inherit
    OP_METHOD,              // [idx:8][ar:8]  | Define method at vtable[idx] with arity
    OP_METHOD_LONG,         // [idx:16][ar:8] | Define method at vtable[idx] with arity

    // ========================================================================
    // COLLECTIONS
    // ========================================================================
    OP_BUILD_LIST,          // [n:8]          | Pop n items, push list
    OP_BUILD_TABLE,         // [n:8]          | Pop n key-value pairs, push table
    OP_INDEX_GET,           //                | Push pop()[pop()] (key, obj -> value)
    OP_INDEX_SET,           //                | obj[key] = val; push val

    // ========================================================================
    // MODULES
    // ========================================================================
    OP_IMPORT,              // [path:8]       | Import module, push it
    OP_IMPORT_LONG          // [path:16]      | Import module, push it

} OpCode;

typedef struct {
    int count;
    int capacity;
    uint8_t* code;
    int* lines;
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(struct VM* vm, Chunk* chunk);
void writeChunk(struct VM* vm, Chunk* chunk, uint8_t byte, int line);
int addConstant(struct VM* vm, Chunk* chunk, Value value);

#endif