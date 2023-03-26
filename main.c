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
#include "error.c"
#include "lex.c"
#include "ast.h"
#include "ast.c"
#include "print.c"
#include "parse.c"
#include "type.c"
#include "resolve.c"
#include "ion.c"

void run_tests(void) {
    init_keywords();

	da_test();
    str_intern_test();
	lex_test();
    // parse_test();
    // resolve_test();
    type_intern_test();
	compile_file("..\tests\resolve_test.ion");
    // printf("Tests Succeeded.\n");
}

int main(int argc, char **argv) {
    (void)argc; (void)argv;
	//run_tests();

	if (argc < 2) {
		printf("usage: %s <filepath>\n", argv[0]);
		return 1;
	}
  	
	compile_file(argv[1]);

    return 0;
}