// A chunk is a sequence of bytecode instructions plus a constant pool and
// line number metadata. One chunk per function. Variable-length encoding:
// each opcode is followed by zero or more operand bytes (most are 1-3 bytes).

#ifndef btl_chunk_h
#define btl_chunk_h

#include "common.h"
#include "value.h"

// ----------------------------------------------------------------------------
// OpCode Enumeration
//
// Each opcode is a single byte. The comment shows:
//   [operands]  | description
//
// Operand notation:
//   idx:8   = 8-bit constant pool index
//   idx:16  = 16-bit constant pool index (big-endian)
//   slot:8  = 8-bit local variable slot
//   n:8     = 8-bit count
//   offset:16 = 16-bit jump offset (big-endian)
//   name:8  = 8-bit constant pool index for identifier
//   argc:8  = 8-bit argument count
//   ic:8    = 8-bit inline cache slot index
//   ar:8    = 8-bit arity
// ----------------------------------------------------------------------------
typedef enum {
    // ========================================================================
    // CONSTANTS & LITERALS
    //
    // Push constant values onto the stack
    // ========================================================================
    BTL_OP_CONSTANT,            // [idx:8]        | Push constants[idx]
    BTL_OP_CONSTANT_LONG,       // [idx:16]       | Push constants[idx]
    BTL_OP_NULL,                //                | Push null
    BTL_OP_TRUE,                //                | Push true
    BTL_OP_FALSE,               //                | Push false
    BTL_OP_0,                   //                | Push 0.0
    BTL_OP_1,                   //                | Push 1.0
    BTL_OP_2,                   //                | Push 2.0
    BTL_OP_INT_0,               //                | Push int 0
    BTL_OP_INT_1,               //                | Push int 1
    BTL_OP_INT_2,               //                | Push int 2

    // ========================================================================
    // STACK MANIPULATION
    //
    // Basic stack operations
    // ========================================================================
    BTL_OP_POP,                 //                | Pop top value
    BTL_OP_POP_N,               // [n:8]          | Pop n values
    BTL_OP_DUP,                 //                | Duplicate top value
    BTL_OP_SWAP,                //                | Swap top two values

    // ========================================================================
    // LOCAL VARIABLES
    //
    // Access variables in the current call frame's slot array.
    // Specialized opcodes for slots 0-7 save one byte per access.
    // ========================================================================
    BTL_OP_GET_LOCAL,           // [slot:8]       | Push locals[slot]
    BTL_OP_GET_LOCAL_0,         //                | Push locals[0]
    BTL_OP_GET_LOCAL_1,         //                | Push locals[1]
    BTL_OP_GET_LOCAL_2,         //                | Push locals[2]
    BTL_OP_GET_LOCAL_3,         //                | Push locals[3]
    BTL_OP_GET_LOCAL_4,         //                | Push locals[4]
    BTL_OP_GET_LOCAL_5,         //                | Push locals[5]
    BTL_OP_GET_LOCAL_6,         //                | Push locals[6]
    BTL_OP_GET_LOCAL_7,         //                | Push locals[7]
    BTL_OP_SET_LOCAL,           // [slot:8]       | locals[slot] = peek(0)
    BTL_OP_SET_LOCAL_0,         //                | locals[0] = peek(0)
    BTL_OP_SET_LOCAL_1,         //                | locals[1] = peek(0)
    BTL_OP_SET_LOCAL_2,         //                | locals[2] = peek(0)
    BTL_OP_SET_LOCAL_3,         //                | locals[3] = peek(0)
    BTL_OP_SET_LOCAL_4,         //                | locals[4] = peek(0)
    BTL_OP_SET_LOCAL_5,         //                | locals[5] = peek(0)
    BTL_OP_SET_LOCAL_6,         //                | locals[6] = peek(0)
    BTL_OP_SET_LOCAL_7,         //                | locals[7] = peek(0)
    BTL_OP_SET_LOCAL_0_POP,     //                | locals[0] = pop()
    BTL_OP_SET_LOCAL_1_POP,     //                | locals[1] = pop()
    BTL_OP_SET_LOCAL_2_POP,     //                | locals[2] = pop()
    BTL_OP_SET_LOCAL_3_POP,     //                | locals[3] = pop()
    BTL_OP_SET_LOCAL_4_POP,     //                | locals[4] = pop()
    BTL_OP_SET_LOCAL_5_POP,     //                | locals[5] = pop()
    BTL_OP_SET_LOCAL_6_POP,     //                | locals[6] = pop()
    BTL_OP_SET_LOCAL_7_POP,     //                | locals[7] = pop()

    // ========================================================================
    // INCREMENT / DECREMENT
    //
    // Specialized opcodes for common ++/-- patterns
    // ========================================================================
    BTL_OP_INC_LOCAL_POP,       // [slot:8]       | locals[slot]++
    BTL_OP_INC_LOCAL,           // [slot:8]       | Push ++locals[slot]
    BTL_OP_INCREMENT,           //                | Push pop() + 1
    BTL_OP_DECREMENT,           //                | Push pop() - 1

    // ========================================================================
    // GLOBAL VARIABLES
    //
    // Access module-level global variables by index
    // ========================================================================
    BTL_OP_GET_GLOBAL,          // [idx:8]        | Push globals[idx]
    BTL_OP_GET_GLOBAL_LONG,     // [idx:16]       | Push globals[idx]
    BTL_OP_DEFINE_GLOBAL,       // [idx:8]        | globals[idx] = pop()
    BTL_OP_DEFINE_GLOBAL_LONG,  // [idx:16]       | globals[idx] = pop()
    BTL_OP_SET_GLOBAL,          // [idx:8]        | globals[idx] = peek(0)
    BTL_OP_SET_GLOBAL_LONG,     // [idx:16]       | globals[idx] = peek(0)

    // ========================================================================
    // UPVALUES (Closures)
    //
    // Upvalues capture variables from enclosing scopes. They can be:
    // - Open: pointing to a stack slot (variable still on stack)
    // - Closed: storing the value directly (variable went out of scope)
    // - Immutable: compile-time constant that never changes
    //
    // Generic opcodes self-patch to specific variants on first access.
    // ========================================================================
    BTL_OP_GET_UPVALUE,         // [slot:8]       | Push upvalue[slot]
    BTL_OP_GET_UPVALUE_OPEN,    // [slot:8]       | Push *upvalue[slot].stack
    BTL_OP_GET_UPVALUE_CLOSED,  // [slot:8]       | Push upvalue[slot].box->closed
    BTL_OP_GET_UPVALUE_IMMUTABLE,// [slot:8]      | Push upvalue[slot].immValue
    BTL_OP_SET_UPVALUE,         // [slot:8]       | upvalue[slot] = peek(0)
    BTL_OP_SET_UPVALUE_OPEN,    // [slot:8]       | *upvalue[slot].stack = peek(0)
    BTL_OP_SET_UPVALUE_CLOSED,  // [slot:8]       | upvalue[slot].box->closed = peek(0)

    // Specialized upvalue opcodes for slots 0-3 (no operand byte needed)
    BTL_OP_GET_UPVALUE_0,
    BTL_OP_GET_UPVALUE_OPEN_0,
    BTL_OP_GET_UPVALUE_CLOSED_0,
    BTL_OP_GET_UPVALUE_IMMUTABLE_0,
    BTL_OP_SET_UPVALUE_0,
    BTL_OP_SET_UPVALUE_OPEN_0,
    BTL_OP_SET_UPVALUE_CLOSED_0,
    BTL_OP_GET_UPVALUE_1,
    BTL_OP_GET_UPVALUE_OPEN_1,
    BTL_OP_GET_UPVALUE_CLOSED_1,
    BTL_OP_GET_UPVALUE_IMMUTABLE_1,
    BTL_OP_SET_UPVALUE_1,
    BTL_OP_SET_UPVALUE_OPEN_1,
    BTL_OP_SET_UPVALUE_CLOSED_1,
    BTL_OP_GET_UPVALUE_2,
    BTL_OP_GET_UPVALUE_OPEN_2,
    BTL_OP_GET_UPVALUE_CLOSED_2,
    BTL_OP_GET_UPVALUE_IMMUTABLE_2,
    BTL_OP_SET_UPVALUE_2,
    BTL_OP_SET_UPVALUE_OPEN_2,
    BTL_OP_SET_UPVALUE_CLOSED_2,
    BTL_OP_GET_UPVALUE_3,
    BTL_OP_GET_UPVALUE_OPEN_3,
    BTL_OP_GET_UPVALUE_CLOSED_3,
    BTL_OP_GET_UPVALUE_IMMUTABLE_3,
    BTL_OP_SET_UPVALUE_3,
    BTL_OP_SET_UPVALUE_OPEN_3,
    BTL_OP_SET_UPVALUE_CLOSED_3,

    // ========================================================================
    // FIELDS & PROPERTIES
    //
    // Object field access. Uses inline caching for performance.
    // ========================================================================
    BTL_OP_FIELD,               // [name_idx:8]   | Register field name on class
    BTL_OP_GET_FIELD_THIS,      // [idx:8]        | Push this.fields[idx]
    BTL_OP_SET_FIELD_THIS,      // [idx:8]        | this.fields[idx] = peek(0)
    BTL_OP_GET_PROPERTY_IC,     // [name:8][ic:8] | Push receiver.property (inline cached)
    BTL_OP_SET_PROPERTY_IC,     // [name:8][ic:8] | receiver.property = peek(0) (inline cached)
    BTL_OP_GET_SUPER,           // [name:8]       | Push bound method from superclass
    BTL_OP_GET_SUPER_LONG,      // [name:16]      | Push bound method from superclass

    // ========================================================================
    // COMPARISON & ARITHMETIC
    //
    // Binary and unary operators. All operate on stack values.
    // ========================================================================
    BTL_OP_EQUAL,               //                | Push pop() == pop()
    BTL_OP_GREATER,             //                | Push pop() > pop()  (b > a)
    BTL_OP_LESS,                //                | Push pop() < pop()  (b < a)
    BTL_OP_ADD,                 //                | Push pop() + pop()
    BTL_OP_SUBTRACT,            //                | Push b - a (a=pop, b=pop)
    BTL_OP_MULTIPLY,            //                | Push pop() * pop()
    BTL_OP_DIVIDE,              //                | Push b / a
    BTL_OP_MODULO,              //                | Push b % a
    BTL_OP_NOT,                 //                | Push !pop()
    BTL_OP_NEGATE,              //                | Push -pop()

    // ========================================================================
    // CONTROL FLOW
    //
    // Conditional and unconditional jumps for loops, if/else, etc.
    // Offsets are 16-bit big-endian.
    // ========================================================================
    BTL_OP_JUMP,                // [offset:16]    | ip += offset
    BTL_OP_JUMP_IF_FALSE,       // [offset:16]    | if falsey(peek(0)) ip += offset
    BTL_OP_POP_JUMP_IF_FALSE,   // [offset:16]    | if falsey(pop()) ip += offset
    BTL_OP_JUMP_IF_TRUE,        // [offset:16]    | if truthy(peek(0)) ip += offset
    BTL_OP_POP_JUMP_IF_TRUE,    // [offset:16]    | if truthy(pop()) ip += offset
    BTL_OP_JUMP_IF_NOT_EQUAL,   // [offset:16]    | if pop() != pop() ip += offset
    BTL_OP_JUMP_IF_EQUAL,       // [offset:16]    | if pop() == pop() ip += offset
    BTL_OP_JUMP_IF_NOT_GREATER, // [offset:16]    | if !(b > a) ip += offset
    BTL_OP_JUMP_IF_NOT_LESS,    // [offset:16]    | if !(b < a) ip += offset
    BTL_OP_LOOP,                // [offset:16]    | ip -= offset (backward jump)

    // ========================================================================
    // FUNCTION CALLS
    //
    // Call functions and closures. Specialized opcodes for 0-8 arguments.
    // Stack layout: [callee, arg0, arg1, ...] -> [result]
    // ========================================================================
    BTL_OP_CALL_0,              //                | Call peek(0) with 0 args
    BTL_OP_CALL_1,              //                | Call peek(1) with 1 arg
    BTL_OP_CALL_2,              //                | Call peek(2) with 2 args
    BTL_OP_CALL_3,              //                | Call peek(3) with 3 args
    BTL_OP_CALL_4,              //                | Call peek(4) with 4 args
    BTL_OP_CALL_5,              //                | Call peek(5) with 5 args
    BTL_OP_CALL_6,              //                | Call peek(6) with 6 args
    BTL_OP_CALL_7,              //                | Call peek(7) with 7 args
    BTL_OP_CALL_8,              //                | Call peek(8) with 8 args
    BTL_OP_CALL,                // [argc:8]       | Call peek(argc) with argc args

    // Tail calls: reuse current frame (avoids stack growth for recursion)
    BTL_OP_TAIL_CALL_0,
    BTL_OP_TAIL_CALL_1,
    BTL_OP_TAIL_CALL_2,
    BTL_OP_TAIL_CALL_3,
    BTL_OP_TAIL_CALL_4,
    BTL_OP_TAIL_CALL_5,
    BTL_OP_TAIL_CALL_6,
    BTL_OP_TAIL_CALL_7,
    BTL_OP_TAIL_CALL_8,
    BTL_OP_TAIL_CALL,           // [argc:8]       | Tail call with argc args

    // ========================================================================
    // METHOD INVOCATION (Indexed - compile-time resolved)
    //
    // Call methods by vtable index. More efficient than name lookup.
    // Stack layout: [receiver, arg0, arg1, ...] -> [result]
    // ========================================================================
    BTL_OP_INVOKE_0,            // [idx:8]        | Call receiver.methods[idx] with 0 args
    BTL_OP_INVOKE_1,            // [idx:8]        | Call receiver.methods[idx] with 1 arg
    BTL_OP_INVOKE_2,            // [idx:8]        | Call receiver.methods[idx] with 2 args
    BTL_OP_INVOKE_3,            // [idx:8]        | Call receiver.methods[idx] with 3 args
    BTL_OP_INVOKE_4,            // [idx:8]        | Call receiver.methods[idx] with 4 args
    BTL_OP_INVOKE_5,            // [idx:8]        | Call receiver.methods[idx] with 5 args
    BTL_OP_INVOKE_6,            // [idx:8]        | Call receiver.methods[idx] with 6 args
    BTL_OP_INVOKE_7,            // [idx:8]        | Call receiver.methods[idx] with 7 args
    BTL_OP_INVOKE_8,            // [idx:8]        | Call receiver.methods[idx] with 8 args

    // Tail invoke (indexed)
    BTL_OP_TAIL_INVOKE_0,
    BTL_OP_TAIL_INVOKE_1,
    BTL_OP_TAIL_INVOKE_2,
    BTL_OP_TAIL_INVOKE_3,
    BTL_OP_TAIL_INVOKE_4,
    BTL_OP_TAIL_INVOKE_5,
    BTL_OP_TAIL_INVOKE_6,
    BTL_OP_TAIL_INVOKE_7,
    BTL_OP_TAIL_INVOKE_8,

    // Invoke with variable arg count (indexed)
    BTL_OP_INVOKE,              // [idx:8][argc:8]      | Call methods[idx] with argc args
    BTL_OP_INVOKE_LONG,         // [idx:16][argc:8]     | Call methods[idx] with argc args

    // ========================================================================
    // METHOD INVOCATION (IC - runtime name lookup with inline caching)
    //
    // Call methods by name with inline caching for performance.
    // Falls back to hash table lookup on cache miss.
    // ========================================================================
    BTL_OP_INVOKE_IC,           // [name:8][argc:8][ic:8] | Call by name, cache result

    // Tail invoke with variable arg count (indexed)
    BTL_OP_TAIL_INVOKE,         // [idx:8][argc:8]      | Tail call methods[idx]
    BTL_OP_TAIL_INVOKE_LONG,    // [idx:16][argc:8]     | Tail call methods[idx]

    // Tail invoke IC
    BTL_OP_TAIL_INVOKE_IC,      // [name:8][argc:8][ic:8] | Tail call by name, cache result

    // ========================================================================
    // SUPER METHOD INVOCATION
    //
    // Call methods on the superclass. Always indexed (resolved at compile time).
    // Stack layout: [this, super, arg0, arg1, ...] -> [result]
    // ========================================================================
    BTL_OP_SUPER_INVOKE_0,      // [idx:8]        | Call super.methods[idx] with 0 args
    BTL_OP_SUPER_INVOKE_1,
    BTL_OP_SUPER_INVOKE_2,
    BTL_OP_SUPER_INVOKE_3,
    BTL_OP_SUPER_INVOKE_4,
    BTL_OP_SUPER_INVOKE_5,
    BTL_OP_SUPER_INVOKE_6,
    BTL_OP_SUPER_INVOKE_7,
    BTL_OP_SUPER_INVOKE_8,

    // Tail super invoke
    BTL_OP_TAIL_SUPER_INVOKE_0,
    BTL_OP_TAIL_SUPER_INVOKE_1,
    BTL_OP_TAIL_SUPER_INVOKE_2,
    BTL_OP_TAIL_SUPER_INVOKE_3,
    BTL_OP_TAIL_SUPER_INVOKE_4,
    BTL_OP_TAIL_SUPER_INVOKE_5,
    BTL_OP_TAIL_SUPER_INVOKE_6,
    BTL_OP_TAIL_SUPER_INVOKE_7,
    BTL_OP_TAIL_SUPER_INVOKE_8,

    // Super invoke with variable arg count
    BTL_OP_SUPER_INVOKE,        // [idx:8][argc:8]      | Call super.methods[idx]
    BTL_OP_SUPER_INVOKE_LONG,   // [idx:16][argc:8]     | Call super.methods[idx]
    BTL_OP_TAIL_SUPER_INVOKE,   // [idx:8][argc:8]      | Tail call super.methods[idx]
    BTL_OP_TAIL_SUPER_INVOKE_LONG, // [idx:16][argc:8]  | Tail call super.methods[idx]

    // ========================================================================
    // CLOSURES
    //
    // Create closures that capture variables from enclosing scopes.
    // Followed by upvalue descriptors: [isLocal:8][index:8][isConst:8] per upvalue
    // ========================================================================
    BTL_OP_CLOSURE,             // [fn:8] [upvals...] | Create closure from constants[fn]
    BTL_OP_CLOSURE_LONG,        // [fn:16][upvals...] | Create closure from constants[fn]
    BTL_OP_CLOSE_UPVALUE,       //                    | Close upvalues for top stack slot, pop

    // ========================================================================
    // RETURN
    // ========================================================================
    BTL_OP_RETURN,              //                | Return pop() from current function

    // ========================================================================
    // CLASSES
    //
    // Create and configure class objects
    // ========================================================================
    BTL_OP_CLASS,               // [name:8]       | Create class, push it
    BTL_OP_CLASS_LONG,          // [name:16]      | Create class, push it
    BTL_OP_INHERIT,             //                | Pop subclass, pop superclass, inherit
    BTL_OP_METHOD,              // [idx:8][ar:8]  | Define method at vtable[idx] with arity
    BTL_OP_METHOD_LONG,         // [idx:16][ar:8] | Define method at vtable[idx] with arity

    // ========================================================================
    // COLLECTIONS
    //
    // Build and access lists and tables (dictionaries)
    // ========================================================================
    BTL_OP_BUILD_LIST,          // [n:8]          | Pop n items, push list
    BTL_OP_BUILD_TABLE,         // [n:8]          | Pop n key-value pairs, push table
    BTL_OP_INDEX_GET,           //                | Push pop()[pop()] (key, obj -> value)
    BTL_OP_INDEX_SET,           //                | obj[key] = val; push val

    // ========================================================================
    // MODULES
    //
    // Import external modules
    // ========================================================================
    BTL_OP_IMPORT,              // [path:8]       | Import module, push it
    BTL_OP_IMPORT_LONG,         // [path:16]      | Import module, push it

    // ========================================================================
    // ACTORS (Concurrency)
    //
    // Create actors and send messages
    // ========================================================================
    BTL_OP_DO_NEW,              //                | do Class(args) - create actor
    BTL_OP_DO_INVOKE,           //                | do actor.method(args) - send message

    // ========================================================================
    // ITERATION (for...in)
    //
    // Iterator opcodes for for-in loops over lists and tables.
    // Stack layout during iteration:
    //   [..., loop_var, collection, index]
    // ========================================================================
    BTL_OP_ITER_INIT,           //                | Validate collection, push index 0
    BTL_OP_ITER_NEXT,           // [slot:8][offset:16] | Advance iterator or jump to exit

} BtlOpCode;

// ----------------------------------------------------------------------------
// BtlChunk Structure
//
// A chunk holds compiled bytecode for a single function. It contains:
// - code: Dynamic array of bytecode bytes
// - lines: Parallel array mapping each byte to source line number
// - constants: Pool of constant values referenced by the bytecode
// ----------------------------------------------------------------------------
typedef struct {
    int count;           // Number of bytes in code array
    int capacity;        // Allocated capacity of code array
    uint8_t* code;       // Bytecode instructions
    int* lines;          // Source line numbers (one per byte)
    BtlValueArray constants;  // Constant pool
} BtlChunk;

// ----------------------------------------------------------------------------
// Chunk Operations
// ----------------------------------------------------------------------------

// Initialize a chunk to empty state
void btl_chunk_init(BtlChunk* chunk);

// Free all memory used by a chunk
void btl_chunk_free(struct VM* vm, BtlChunk* chunk);

// Write a byte to the chunk with line number info
void btl_chunk_write(struct VM* vm, BtlChunk* chunk, uint8_t byte, int line);

// Add a constant to the chunk's constant pool, return its index
int btl_chunk_add_constant(struct VM* vm, BtlChunk* chunk, BtlValue value);

#endif // btl_chunk_h
