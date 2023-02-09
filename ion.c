#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <math.h>


#include "common.c"
#include "lexer.c"
#include "ast.h"
#include "ast.c"
#include "parser.c"


void run_tests(void) {
	da_test();
    str_intern_test();
	lex_test();
    parse_test();
    printf("Tests Succeeded.\n");
}

int main(int argc, char **argv) {
    (void)argc; (void)argv;
    run_tests();
}
