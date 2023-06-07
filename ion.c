void parse_file(void) {
	while (token.kind != TOKEN_EOF) {
		Decl *decl = parse_decl();
		if (decl->kind == DECL_DIRECTIVE) {
			da_push(directives, decl);
		} else {
			sym_put_decl(decl);
		}
	}
}

int compile_file(char *path) {

	FILE *fp = fopen(path, "r");
	if (!fp) {
		printf("error: failed to open %s: %s\n", path, strerror(errno));
		return 1;
	}

	compilation_filepath = path;
	char *out_filepath = replace_ext(path, "c");
	if (!out_filepath) {
		// if there is no extension on the filepath passed in, just append .c
		out_filepath = strf("%s.c", path);
	}

	char *file_data;
	size_t file_size;
	int rc = read_entire_file(fp, &file_data, &file_size);
	if (rc != READ_ENTIRE_FILE_OK) {
		fprintf(stderr, "error: failed to read file %s\n", path);
		fclose(fp);
		return 1;
	}
	fclose(fp);

	init_keywords();
	sym_init_table();
	init_stream(path, file_data);

	// parsing
	parse_file();

	// semantic analysis
	for (int i = 0; i<da_len(directives); ++i) {
		resolve_decl_directive(directives[i]);
	}
	for (int i = 0; i<da_len(global_syms_buf); ++i) {
		complete_sym(global_syms_buf[i]);
	}

	// code generation
	FILE *out_file = fopen(out_filepath, "w");
	if (!out_file) {
		fprintf(stderr, "error: failed to open output file %s\n", out_filepath);
		return 1;
	}
	gen_all_c(out_file);
	fclose(out_file);

	printf("Compilation succeeded: %s\n", out_filepath);

	return 0;
}
