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

	// parse decls until you reach end of file
	while (token.kind != TOKEN_EOF) {
		Decl *decl = parse_decl();
		sym_put_decl(decl);
	}
	
	for (int i = 0; i<da_len(global_syms); ++i) {
		complete_sym(global_syms[i]);
	}

	char *preamble = gen_preamble_c();
	if (preamble) printf("%s\n", preamble);

	char *forward_decls = gen_forward_decls_c(global_syms);
	if (forward_decls) printf("%s\n", forward_decls);

	for (int i = 0; i<da_len(ordered_syms); ++i) {
		Sym *sym = ordered_syms[i];
		char *str = gen_sym_c(ordered_syms[i]);
		if (str) printf("%s\n", str);
	}

	return 0;
}
