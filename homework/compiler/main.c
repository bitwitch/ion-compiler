#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>

#include "da.c"
#include "vm.c"
#include "parser.c"


#define test_compile_and_run(expr) \
do { \
    AST_Node *head = parse_expression(#expr); \
    uint8_t *code = generate_bytecode(head); \
    uint32_t result = vm_exec(code); \
    assert(vm_exec(code) == (expr)); \
} while(0)


int main(int argc, char **argv) {
	(void)argc; (void)argv;

    AST_Node *head = parse_expression("~~~~~~~~~~~~~~~42");
    print_s_expression(head);
    uint8_t *code = generate_bytecode(head);
    uint32_t result = vm_exec(code);
    printf("result = %d\n", result);


    test_compile_and_run(60 + 9);
    test_compile_and_run(12*34 + 45/8 + ~25);
    test_compile_and_run(420 * (123 - 45) / (66 - 12 + 2) + 1);
    test_compile_and_run(70 + -1);
    test_compile_and_run(-5 - -4);
    test_compile_and_run(-(-(-(-(-(-(-(-(-(-69))))))))));
    test_compile_and_run(~~~~~~~~~~~~~~~42);


	return 0;
}



