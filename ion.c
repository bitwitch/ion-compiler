int compile_file(char *path) {

	FILE *fp = fopen(path, "r");
	if (!fp) {
		fprintf(stderr, "error: failed to open %s: %s\n", path, strerror(errno));
		return 1;
	}

	compilation_filepath = path;

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

	// insert primative types into the symbol table at startup 
	sym_put_type(str_intern("void"), type_void);
	sym_put_type(str_intern("int"), type_int);
	sym_put_type(str_intern("float"), type_float);
	sym_put_type(str_intern("char"), type_char);
	sym_put_type(str_intern("bool"), type_bool);

	init_stream(path, file_data);

	// parse decls until you reach end of file
	while (token.kind != TOKEN_EOF) {
		Decl *decl = parse_decl();
		sym_put_decl(decl);
	}
	
	for (int i = 0; i<da_len(global_syms); ++i) {
		complete_sym(global_syms[i]);
	}

	for (int i = 0; i<da_len(ordered_syms); ++i) {
		Sym *sym = ordered_syms[i];
		print_decl(sym->decl);
		printf("\n");
	}

	return 0;
}
