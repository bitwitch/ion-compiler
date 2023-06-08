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
#include <errno.h>
#include <limits.h>

#include "common.c"
#include "os.c"
#include "error.c"
#include "lex.c"
#include "ast.h"
#include "ast.c"
#include "print.c"
#include "parse.c"
#include "type.c"
#include "resolve.c"
#include "codegen.c"
#include "ion.c"

void run_tests(void) {
	init_keywords();
	sym_init_table();

	// read_dir_test();
	// da_test();
	// map_test();
	// str_intern_test();
	// lex_test();
	// parse_test();
	// type_intern_test();
	// resolve_test();
	// codegen_test();

}

int main(int argc, char **argv) {
    (void)argc; (void)argv;
	// run_tests();

	if (argc < 2) {
		printf("usage: %s <ion-package> [outfile]\n", argv[0]);
		return 1;
	}

	if (!compile_package(argv[1])) {
		fprintf(stderr, "Failed to compile package %s\n", argv[1]);
		return 1;
	}

	return 0;
}
