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

int main(int argc, char **argv) {
	(void)argc; (void)argv;
    vm_tests();
    /*parse_test();*/

    /*AST_Node *head = parse_expression("12*34 + 45/56 + ~25");*/
    AST_Node *head = parse_expression("60 + 9");
    print_s_expression(head); printf("\n");
    uint8_t *code = generate_bytecode(head);
    uint32_t result = vm_exec(code);
    printf("result = %d\n", result);

	return 0;
}



