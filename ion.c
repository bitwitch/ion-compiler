#define MAX_SEARCH_PATHS 256
char *package_search_paths[MAX_SEARCH_PATHS];
int num_package_search_paths;

void add_package_search_path(char *path) {
	path_normalize(path);
	printf("Adding package search path %s\n", path);
	package_search_paths[num_package_search_paths++] = str_intern(path);
}

void add_package_search_path_range(char *start, char *end) {
	char path[MAX_PATH];
	size_t len = MIN(end - start, MAX_PATH - 1);
	memcpy(path, start, len);
	path[len] = 0;
	add_package_search_path(path);
}

void init_package_search_paths(void) {
	char *ionhome_var = getenv("IONHOME");
	if (!ionhome_var) {
		fprintf(stderr, "error: Set the environment variable IONHOME to the Ion home directory (where system_packages is located)\n");
		exit(1);
	}
	char path[MAX_PATH];
	path_copy(path, ionhome_var);
	path_join(path, "system_packages");
	add_package_search_path(path);
	add_package_search_path(".");
	char *ionpath_var = getenv("IONPATH");
	if (ionpath_var) {
		char *start = ionpath_var;
		for (char *ptr = ionpath_var; *ptr; ++ptr) {
			if (*ptr == ';') {
				add_package_search_path_range(start, ptr);
				start = ptr + 1;
			}
		}
		if (*start) {
			add_package_search_path(start);
		}
	}
}

bool is_package_dir(char *search_path, char *package_path) {
	bool result = false;
	char path[MAX_PATH];
	path_copy(path, search_path);
	path_join(path, package_path);

	// if there are any .ion files in this path, consider the package directory found
	DirEntry *entries = read_dir(path);
	for (int i=0; i<da_len(entries); ++i) {
		DirEntry entry = entries[i];
		char *ext = path_ext(entry.name);
		if (ext != entry.name && strcmp(ext, "ion") == 0) {
			result = true;
			break;
		}
	}

	da_free(entries);
	return result;
}

bool has_foreign_sources(Package *package) {
	for (int i=0; i<da_len(package->directives); ++i) {
		assert(package->directives[i]->kind == DECL_DIRECTIVE);
		NoteArg *args = package->directives[i]->directive.args;
		int num_args = package->directives[i]->directive.num_args;
		for (int j=0; j<num_args; ++j) {
			if (args[j].name == name_source) {
				return true;
			}
		}
	}
	return false;
}

bool copy_package_full_path(char dest[MAX_PATH], char *package_path) {
	for (int i=0; i<num_package_search_paths; ++i) {
		if (is_package_dir(package_search_paths[i], package_path)) {
			path_copy(dest, package_search_paths[i]);
			path_join(dest, package_path);
			return true;
		}
	}
	return false;
}

bool parse_package_file(Package *package, char *filepath) {
	FILE *fp = fopen(filepath, "r");
	if (!fp) {
		fprintf(stderr, "error: failed to open %s: %s\n", filepath, strerror(errno));
		return false;
	}

	char *file_data;
	size_t file_size;
	int rc = read_entire_file(fp, &file_data, &file_size);
	if (rc != READ_ENTIRE_FILE_OK) {
		fprintf(stderr, "error: failed to read file %s\n", filepath);
		fclose(fp);
		return false;
	}
	fclose(fp);

	init_stream(filepath, file_data);

	while (token.kind != TOKEN_EOF) {
		Decl *decl = parse_decl();
		da_push(package->decls, decl);
		if (decl->kind == DECL_DIRECTIVE) {
			da_push(package->directives, decl);
		}
	}

	return true;
}

void add_package_decls_to_sym_table(Package *package) {
	for (int i=0; i<da_len(package->decls); ++i) {
		Decl *decl = package->decls[i];
		if (decl->kind != DECL_DIRECTIVE && decl->kind != DECL_IMPORT) {
			sym_put_decl(decl);
		}
	}
}

void import_all_package_symbols(Package *package) {
	for (int i=0; i<da_len(package->syms); ++i) {
		Sym *sym = package->syms[i];
		// ignore main function in imported packages to allow using package as executable and as a library
		if (sym->kind == SYM_FUNC && sym->name == str_intern("main")) {
			continue;
		}
		if (sym->package == package) {
			sym_package_put(sym->name, sym);
		}
	}
}


void add_package(Package *package) {
	Package *old_package = map_get(&package_map, package->path);
	if (old_package != package) {
		assert(!old_package);
		da_push(packages, package);
		map_put(&package_map, package->path, package);
	}
}

bool parse_package(Package *package);

// create new package or return cached one, parse all of the ion files in it,
// and add symbols to the package symbol table
Package *import_package(char *package_path) {
	package_path = str_intern(package_path);
	Package *package = map_get(&package_map, package_path);
	if (!package) {
		package = xcalloc(1, sizeof(Package));
		package->path = package_path;
		printf("Importing %s\n", package_path);
		if (!copy_package_full_path(package->full_path, package_path)) {
			free(package);
			return NULL;
		}
		char *external_name = str_replace_char(package_path, '/', '_');
		package->external_name = str_intern(external_name);
		add_package(package);
		parse_package(package);
	}
	return package;
}

void import_package_symbols(Decl *decl, Package *package) {
	assert(decl->kind == DECL_IMPORT);
	for (int i=0; i<decl->import.num_items; ++i) {
		ImportItem item = decl->import.items[i];
		Sym *sym = get_package_sym(package, item.name);
        if (!sym) {
            semantic_error(decl->pos, "symbol '%s' does not exist in package '%s'", item.name, package->path);
		}
		if (sym->package != package) {
            semantic_error(decl->pos, "symbol '%s' is not native to package '%s', you must import it from its native package '%s'", 
				item.name, package->path, sym->package->path);
		}
		sym_package_put(item.rename ? item.rename : item.name, sym);
	}
}

void process_package_imports(Package *package) {
	for (int i=0; i<da_len(package->decls); ++i) {
		Decl *decl = package->decls[i];
		if (decl->kind == DECL_IMPORT) {
			BUF(char *path) = NULL;
			if (decl->import.is_relative) {
				da_printf(path, "%s/", package->path);
			}
			da_printf(path, "%s", decl->import.package_path);
			Package *imported_package = import_package(path);
			if (!imported_package) {
				semantic_error(decl->pos, "failed to import package '%s'", path);
			}
			da_free(path);
			import_package_symbols(decl, imported_package);
			if (decl->import.import_all) {
				import_all_package_symbols(imported_package);
			}
		}
	}
}

bool parse_package(Package *package) {
	// parse all .ion files in package directory
	DirEntry *entries = read_dir(package->full_path);
	if (da_len(entries) == 0) {
		fprintf(stderr, "error: directory is empty: %s\n", package->full_path);
		return false;
	}
	char *ext_ion = str_intern("ion");
	bool found_one = false;
	for (int i=0; i<da_len(entries); ++i) {
		if (str_intern(path_ext(entries[i].name)) != ext_ion)
			continue;
		found_one = true;
		char filepath[MAX_PATH];
		path_copy(filepath, package->full_path);
		path_join(filepath, entries[i].name);
		if (!parse_package_file(package, filepath)) { 
			fprintf(stderr, "error: failed to parse file: %s\n", filepath);
			return false;
		}
	}
	if (!found_one) {
		fprintf(stderr, "error: no .ion files found in: %s\n", package->full_path);
		return false;
	}

	// add symbols to package symbol table
	Package *old_package = enter_package(package);
	if (builtin_package) {
		import_all_package_symbols(builtin_package);
	}
	add_package_decls_to_sym_table(package);
	process_package_imports(package);
	leave_package(old_package);
	return true;
}

void init_builtin_types(void) {
	assert(builtin_package);
	Package *old_package = enter_package(builtin_package);

	// primative types
	sym_put_type(str_intern("void"),       type_void);
	sym_put_type(str_intern("char"),       type_char);
	sym_put_type(str_intern("schar"),      type_schar);
	sym_put_type(str_intern("uchar"),      type_uchar);
	sym_put_type(str_intern("short"),      type_short);
	sym_put_type(str_intern("ushort"),     type_ushort);
	sym_put_type(str_intern("int"),        type_int);
	sym_put_type(str_intern("uint"),       type_uint);
	sym_put_type(str_intern("long"),       type_long);
	sym_put_type(str_intern("ulong"),      type_ulong);
	sym_put_type(str_intern("llong"),      type_llong);
	sym_put_type(str_intern("ullong"),     type_ullong);
	sym_put_type(str_intern("float"),      type_float);
	sym_put_type(str_intern("double"),     type_double);
	sym_put_type(str_intern("bool"),       type_bool);

	// built-in constants
	// sym_put_const(str_intern("true"),  type_bool,           (Val){.i=1});
	// sym_put_const(str_intern("false"), type_bool,           (Val){.i=0});
	// sym_put_const(str_intern("NULL"),  type_ptr(type_void), (Val){.p=0});

	leave_package(old_package);
}

void init_builtin_package(void) {
	builtin_package = import_package("builtin");
	if (!builtin_package) {
		fatal("failed to initialize builtins\n");
	}
	builtin_package->external_name = str_intern("");
	builtin_package->always_reachable = true;
	init_builtin_types();
}

void init_compiler(void) {
	init_package_search_paths();
	init_keywords();
	init_builtin_package();
}

bool compile_package(char *package_name, char *out_name) {
	init_compiler();

	char package_path[MAX_PATH];
	path_copy(package_path, package_name);

	// parsing
	Package *main_package = import_package(package_path);
	if (!main_package) {
		fatal("failed to import main package '%s'\n", package_path);
	}

	// semantic analysis
	char *main_name = str_intern("main");
	Sym *main_sym = get_package_sym(main_package, main_name);
	if (!main_sym) {
		fatal("package '%s' contains no function called main", package_name);
	}


	// resolve all symbols in packages that have always_reachable set
	for (int i=0; i<da_len(packages); ++i) {
		if (packages[i]->always_reachable) {
			resolve_package(packages[i]);
		}
	}

	resolve_package(main_package);

	complete_reachable_syms();
    printf("Compiled %d symbols in %d packages\n", da_len(reachable_syms), da_len(packages));

	// ensure that package prefixing does not happen to main() in code gen
	main_sym->external_name = main_name;
	
	// code generation
	char out_filepath[MAX_PATH];
	if (out_name) {
		path_copy(out_filepath, out_name);
	} else {
		snprintf(out_filepath, sizeof(out_filepath), "%s.c", package_path);
	}

	FILE *out_file = fopen(out_filepath, "w");
	if (!out_file) {
		fprintf(stderr, "error: failed to open output file %s\n", out_filepath);
		return false;
	}

	gen_all_c(out_file);

	fclose(out_file);

	printf("Compilation succeeded: %s\n", out_filepath);

	return true;
}




// int compile_file(char *path) {

	// FILE *fp = fopen(path, "r");
	// if (!fp) {
		// fprintf(stderr, "error: failed to open %s: %s\n", path, strerror(errno));
		// return 1;
	// }

	// compilation_filepath = path;
	// char *out_filepath = replace_ext(path, "c");
	// if (!out_filepath) {
		// // if there is no extension on the filepath passed in, just append .c
		// out_filepath = strf("%s.c", path);
	// }

	// char *file_data;
	// size_t file_size;
	// int rc = read_entire_file(fp, &file_data, &file_size);
	// if (rc != READ_ENTIRE_FILE_OK) {
		// fprintf(stderr, "error: failed to read file %s\n", path);
		// fclose(fp);
		// return 1;
	// }
	// fclose(fp);

	// init_keywords();
	// sym_init_table();
	// init_stream(path, file_data);

	// // parsing
	// parse_file();

	// // semantic analysis
	// for (int i = 0; i<da_len(directives); ++i) {
		// resolve_decl_directive(directives[i]);
	// }
	// for (int i = 0; i<da_len(global_syms_buf); ++i) {
		// complete_sym(global_syms_buf[i]);
	// }

	// // code generation
	// FILE *out_file = fopen(out_filepath, "w");
	// if (!out_file) {
		// fprintf(stderr, "error: failed to open output file %s\n", out_filepath);
		// return 1;
	// }
	// gen_all_c(out_file);
	// fclose(out_file);

	// printf("Compilation succeeded: %s\n", out_filepath);

	// return 0;
// }


