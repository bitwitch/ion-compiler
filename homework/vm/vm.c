#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <assert.h>

#define MAX_STACK 2048

#define POP()     (*--top)
#define PUSH(x)   (*top++ = (x))
#define POPS(n)   assert(top - stack >= (n))
#define PUSHES(n) assert(top + (n) <= stack + MAX_STACK)

typedef enum {
    OP_NONE,

    OP_NOT,
    OP_NEG,

    OP_MUL,
    OP_DIV,
    OP_MOD,
    OP_SHL,
    OP_SHR,
    OP_AND,

    OP_ADD,
    OP_SUB,
    OP_OR,
    OP_XOR,

    OP_LIT,
    OP_HALT,
} Opcode;


#define CASE_BINARY(opcode, op) \
    case opcode: \
    { \
        POPS(2); \
        int32_t right = POP(); \
        int32_t left = POP(); \
        PUSHES(1); \
        PUSH(left op right); \
        break; \
    }
 
int32_t vm_exec(uint8_t *code) {
    int32_t stack[MAX_STACK];
    int32_t *top = stack;
    int32_t val;
    for (;;) {
        uint8_t op = *code++;
        switch (op) {
        case OP_NOT: 
            POPS(1);
            val = POP();
            PUSHES(1);
            PUSH(~val);
            break;
        case OP_NEG: 
            POPS(1);
            val = POP();
            PUSHES(1);
            PUSH(-val);
            break;

        CASE_BINARY(OP_MUL, *)
        CASE_BINARY(OP_DIV, /)
        CASE_BINARY(OP_MOD, %)
        CASE_BINARY(OP_SHL, <<)
        CASE_BINARY(OP_SHR, >>)
        CASE_BINARY(OP_AND, &)
        CASE_BINARY(OP_ADD, +)
        CASE_BINARY(OP_SUB, -)
        CASE_BINARY(OP_OR,  |)
        CASE_BINARY(OP_XOR, ^)

        case OP_LIT:
            val = *(int32_t*)code;
            PUSHES(1);
            PUSH(val);
            code += sizeof(int32_t);
            break;
        case OP_HALT:
            POPS(1);
            return POP();
        default:
            assert(0);
            return 0;
        }
    }
    assert(0);
    return 0;
}

#undef CASE_BINARY


// NOTE(shaw): this is not robust, it is just for quickly debugging,
// it assumes a well formed program as input
void make_program(uint8_t *buf, size_t buf_size, ...) {
    Opcode op;
    uint32_t val;
    size_t i = 0;

    va_list args;
    va_start(args, buf_size);

    do {
        op = va_arg(args, Opcode);
        assert(i < buf_size);
        buf[i++] = op;
        if (op == OP_LIT) {
            val = va_arg(args, int32_t);
            int num_bytes = sizeof(val);
            assert(i + num_bytes < buf_size);
            for (; num_bytes > 0; --num_bytes) {
                buf[i++] = val & 0xFF; 
                val >>= 8;
            }
        }
    } while (op != OP_HALT);

    va_end(args);
}

#define test_program(code, result) assert(vm_exec(code) == (result))

void vm_tests(void) {
    /*uint8_t code[256] = {*/
        /*OP_LIT, 0x3c, 0x00, 0x00, 0x00,       // LIT 60*/
        /*OP_LIT, 0x09, 0x00, 0x00, 0x00,       // LIT 9*/
        /*OP_ADD,                               // ADD*/
        /*OP_HALT,                              // HALT*/
    /*};*/

    enum { MAX_CODE = 256 };
    uint8_t code[MAX_CODE];

    make_program(code, 256, OP_LIT, 60, OP_LIT, 9, OP_ADD, OP_HALT);
    test_program(code, 69);
    make_program(code, 256, OP_LIT, 500, OP_LIT, 40, OP_SUB, OP_LIT, 40, OP_SUB, OP_HALT);
    test_program(code, 420);
    make_program(code, 256, OP_LIT, 300, OP_LIT, 2, OP_MUL, OP_LIT, 100, OP_ADD, OP_LIT, 34, OP_SUB, OP_HALT);
    test_program(code, 666);

}

int main(int argc, char **argv) {
    (void)argc; (void)argv;
    vm_tests();
    return 0;
}
