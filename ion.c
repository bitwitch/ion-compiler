int compile_file(char *path) {

	FILE *fp = fopen(path, "r");
	if (!fp) {
		printf("error: failed to open %s: %s\n", path, strerror(errno));
		return 1;
	}

	compilation_filepath = path;

	char *file_data;
	size_t file_size;
	int rc = read_entire_file(fp, &file_data, &file_size);
	if (rc != READ_ENTIRE_FILE_OK) {
		printf("error: failed to read file %s\n", path);
		fclose(fp);
		return 1;
	}
	fclose(fp);

	init_keywords();
	sym_init_table();
	init_stream(path, file_data);

	// parsing
	while (token.kind != TOKEN_EOF) {
		Decl *decl = parse_decl();
		sym_put_decl(decl);
	}
	
	// semantic analysis
	for (int i = 0; i<da_len(global_syms_buf); ++i) {
		complete_sym(global_syms_buf[i]);
	}

	// code generation
	gen_all_c();

	return 0;
}
