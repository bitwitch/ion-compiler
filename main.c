#define _CRT_SECURE_NO_WARNINGS
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
	init_compiler();

	read_dir_test();
	da_test();
	map_test();
	str_intern_test();
	lex_test();
	parse_test();
	type_intern_test();
	codegen_test();

}

int main(int argc, char **argv) {
    (void)argc; (void)argv;
	// run_tests();

	if (argc < 2) {
		printf("usage: %s <ion-package> [outfile]\n", argv[0]);
		return 1;
	}

	char *package_name = argv[1];
	char *out_name = argc >= 3 ? argv[2] : NULL;

	if (!compile_package(package_name, out_name)) {
		fprintf(stderr, "Failed to compile package %s\n", package_name);
		return 1;
	}

	return 0;
}
