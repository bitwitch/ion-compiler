// @LEAK all the strf calls allocate and leak, right now strf calls malloc, but
// will switch to arena allocator in future

#define INDENT_WIDTH 4
static int gen_indent = 0; 

#define gen_newline(b) (b) = gen__newline(b)
char *gen__newline(char *buf) {
    if (gen_indent > 0)
        da_printf(buf, "\n%*s", gen_indent*INDENT_WIDTH, " ");
    else
        da_printf(buf, "\n");
	return buf;
}

char *gen_parens(char *str, bool b) {
	return b ? strf("(%s)", str) : str;
}

char *gen_type_c(Type *type, char *inner) {
	assert(type);
	char *sep = *inner ? " " : "";
	switch(type->kind) {
	case TYPE_VOID:       return strf("void%s%s", sep, inner);
	case TYPE_CHAR:       return strf("char%s%s", sep, inner);
	case TYPE_SCHAR:      return strf("schar%s%s", sep, inner);
	case TYPE_UCHAR:      return strf("uchar%s%s", sep, inner);
	case TYPE_SHORT:      return strf("short%s%s", sep, inner);
	case TYPE_USHORT:     return strf("ushort%s%s", sep, inner);
	case TYPE_INT:        return strf("int%s%s", sep, inner);
	case TYPE_UINT:       return strf("uint%s%s", sep, inner);
	case TYPE_LONG:       return strf("long%s%s", sep, inner);
	case TYPE_ULONG:      return strf("ulong%s%s", sep, inner);
	case TYPE_LONGLONG:   return strf("longlong%s%s", sep, inner);
	case TYPE_ULONGLONG:  return strf("ulonglong%s%s", sep, inner);
	case TYPE_FLOAT:      return strf("float%s%s", sep, inner);
	case TYPE_DOUBLE:     return strf("double%s%s", sep, inner);
	case TYPE_BOOL:       return strf("bool%s%s", sep, inner);

	case TYPE_STRUCT:
	case TYPE_UNION:
	case TYPE_ENUM: {
		return strf("%s%s%s", type->sym->name, sep, inner);
	}

	case TYPE_PTR: {
		// int *(x)
		char *str = gen_parens(strf("*%s", inner), *inner);
		return gen_type_c(type->ptr.base, str);
	}

	case TYPE_ARRAY: {
		// int (x)[8]
		char *str = gen_parens(strf("%s[%d]", inner, type->array.num_items), *inner);
		return gen_type_c(type->array.base, str);
	}

	case TYPE_FUNC: {
		// int (*x)(int)
		BUF(char *params) = NULL; // @LEAK
		int num_params = type->func.num_params;
		if (num_params == 0) {
			da_printf(params, "void");
		}
		for (int i=0; i<num_params; ++i) {
			TypeField param = type->func.params[i];
			da_printf(params, "%s%s", gen_type_c(param.type, ""), i == num_params-1 ? "" : ", ");
		}
		if (type->func.is_variadic) {
			da_printf(params, ", ...");
		}
		char *str = strf("(*%s)(%s)", inner, params);
		return gen_type_c(type->func.ret, str);
	}

	default:
		assert(0);
		return NULL;
	}
}

char *gen_typespec_c(Typespec *typespec, char *inner);

char *gen_expr_c(Expr *expr) {
    assert(expr);
    switch (expr->kind) {
    case EXPR_FLOAT: 
        return strf("%g", expr->float_val);
	case EXPR_BOOL:
		return strf("%s", expr->bool_val ? "true" : "false"); 
    case EXPR_STR: 
        return strf("\"%s\"", expr->str_val); 
    case EXPR_CHAR: 
        return strf("\'%c\'", expr->int_val); 
    case EXPR_NAME: 
        return strf("%s", expr->name); 
    case EXPR_UNARY: 
		return strf("%c(%s)", expr->unary.op, gen_expr_c(expr->unary.expr));
    case EXPR_INT: {
		char *fmt = "%lld";
		if (expr->mod == TOKENMOD_HEX || expr->mod == TOKENMOD_BIN) {
			fmt = "%#llx";
		} else if (expr->mod == TOKENMOD_OCT) {
			fmt = "%#llo";
		}
        return strf(fmt, expr->int_val);
	}
    case EXPR_BINARY: {
		char *op = strf("%s", token_kind_to_str(expr->binary.op));
        return strf("(%s) %s (%s)", 
            gen_expr_c(expr->binary.left),
            op,
            gen_expr_c(expr->binary.right));
	}
    case EXPR_TERNARY: 
        return strf("(%s ? %s : %s)",
            gen_expr_c(expr->ternary.cond),
            gen_expr_c(expr->ternary.then_expr),
            gen_expr_c(expr->ternary.else_expr));
    case EXPR_CALL: {
        char *name = gen_expr_c(expr->call.expr);
        Expr **args = expr->call.args;
        int num_args = expr->call.num_args;

		BUF(char *str) = NULL;
		da_printf(str, "%s(", name);
        for (int i=0; i<num_args; ++i) {
			da_printf(str, "%s%s", gen_expr_c(args[i]), i == num_args - 1 ? "" : ", ");
        }
		da_printf(str, ")");
        return str;
    }
    case EXPR_INDEX: 
        return strf("%s[%s]",
			gen_expr_c(expr->index.expr),
            gen_expr_c(expr->index.index));
    case EXPR_FIELD: 
        return strf("%s.%s",
            gen_expr_c(expr->field.expr),
            expr->field.name);
    case EXPR_COMPOUND: {
        // TODO(shaw): get the decl for the type here so that we can have named fields (designated initializer)
        // (Vec2){ .x = 69, .y = 420 }

		BUF(char *str) = NULL; // @LEAK

		if (expr->type->kind != TYPE_ARRAY) {
			if (expr->compound.typespec) {
				da_printf(str, "(%s)", gen_typespec_c(expr->compound.typespec, ""));
			} else {
				da_printf(str, "(%s)", gen_type_c(expr->type, ""));
			}
		}
		da_printf(str, "{");

        int num_args = expr->compound.num_args;
        for (int i=0; i<num_args; ++i) {
			da_printf(str, "%s%s", 
				gen_expr_c(expr->compound.args[i]),
				i == num_args - 1 ? "" : ", ");
        }
		da_printf(str, "}");
		return str;
    }
	case EXPR_CAST:
		return strf("(%s)(%s)", 
			gen_typespec_c(expr->cast.typespec, ""),
			gen_expr_c(expr->cast.expr));
	case EXPR_SIZEOF_EXPR:
        return strf("sizeof(%s)", gen_expr_c(expr->sizeof_expr));
    case EXPR_SIZEOF_TYPE:
        return strf("sizeof(%s)", gen_typespec_c(expr->sizeof_typespec, ""));
    default:
        printf("Error: Codegen: Unknown expr kind: %d\n", expr->kind);
        assert(0);
        return NULL;
    }
}

char *gen_typespec_c(Typespec *typespec, char *inner) {
	assert(typespec);
	char *sep = *inner ? " " : "";

	switch (typespec->kind) {
	case TYPESPEC_NAME:
        return strf("%s%s%s", typespec->name, sep, inner);

	case TYPESPEC_ARRAY: {
		char *str;
		if (typespec->array.num_items) {
			str = gen_parens(strf("%s[%s]", inner, gen_expr_c(typespec->array.num_items)), *inner);
		} else {
			str = gen_parens(strf("%s[]", inner), *inner);
		}
		return gen_typespec_c(typespec->array.base, str);
	}

	case TYPESPEC_POINTER: {
		char *str = gen_parens(strf("*%s", inner), *inner);
		return gen_typespec_c(typespec->ptr.base, str);
	}

	case TYPESPEC_FUNC: {
		BUF(char *params) = NULL; // @LEAK
		int num_params = typespec->func.num_params;
		if (num_params == 0) {
			da_printf(params, "void");
		}
		for (int i=0; i<num_params; ++i) {
			da_printf(params, "%s%s", gen_typespec_c(typespec->func.params[i], ""), i == num_params-1 ? "" : ", ");
		}
		if (typespec->func.is_variadic) {
			da_printf(params, ", ...");
		}
		char *str = strf("(*%s)(%s)", inner, params);
		return gen_typespec_c(typespec->func.ret, str);
	}

	default: 
		assert(0);
		return NULL;
	}
}

char *gen_forward_decls_c(Sym **global_syms) {
	BUF(char *str) = NULL;

	// forward declare types
	for (int i=0; i<da_len(global_syms); ++i) {
		Sym *sym = global_syms[i];
		// NOTE(shaw): primative types don't have declarations, skip those
		if (!sym->decl) continue;
		if (sym->kind == SYM_TYPE) {
			if (sym->decl->kind == DECL_STRUCT) {
				da_printf(str, "typedef struct %s %s;", sym->name, sym->name);
				gen_newline(str);
			} else if (sym->decl->kind == DECL_UNION) {
				da_printf(str, "typedef union %s %s;", sym->name, sym->name);
				gen_newline(str);
			} else if (sym->decl->kind == DECL_ENUM) {
				da_printf(str, "typedef enum %s %s;", sym->name, sym->name);
				gen_newline(str);
			}
		}
	}
	return str;
}


char *gen_stmt_block_c(StmtBlock block);

char *gen_stmt_c(Stmt *stmt) {
	switch (stmt->kind) {
	case STMT_CONTINUE:
		return "continue";
	case STMT_BREAK:
		return "break";
	case STMT_RETURN:
		return strf("return %s", gen_expr_c(stmt->return_stmt.expr));
	case STMT_BRACE_BLOCK:
		return gen_stmt_block_c(stmt->block);
	case STMT_EXPR:
		return gen_expr_c(stmt->expr);

	case STMT_ASSIGN: {
		char *lhs = gen_expr_c(stmt->assign.left);
		if (stmt->assign.right) {
			return strf("%s %s %s",
				lhs,
				token_kind_to_str(stmt->assign.op),
				gen_expr_c(stmt->assign.right));
		} else {
			return strf("%s%s", lhs, token_kind_to_str(stmt->assign.op));
		}
	}

	case STMT_INIT: {
		char *type = stmt->init.typespec 
			? gen_typespec_c(stmt->init.typespec, stmt->init.name)
			: gen_type_c(stmt->init.expr->type, stmt->init.name);

		if (stmt->init.expr) {
			return strf("%s = %s", type, gen_expr_c(stmt->init.expr));
		} else {
			return strf("%s", type);
		}
	}

	case STMT_IF: {
		BUF(char *str) = NULL;
		da_printf(str, "if (%s) %s", 
				gen_expr_c(stmt->if_stmt.cond),
				gen_stmt_block_c(stmt->if_stmt.then_block));

		ElseIf *else_ifs = stmt->if_stmt.else_ifs; 
		for (int i=0; i < stmt->if_stmt.num_else_ifs; ++i) {
			da_printf(str, " else if (%s) %s", 
				gen_expr_c(else_ifs[i].cond),
				gen_stmt_block_c(else_ifs[i].block));
		}

		StmtBlock else_block = stmt->if_stmt.else_block;
		if (else_block.num_stmts > 0) {
			da_printf(str, " else %s", gen_stmt_block_c(stmt->if_stmt.else_block));
		}
		return str;
	}

	case STMT_FOR: {
		char *init = stmt->for_stmt.init ? gen_stmt_c(stmt->for_stmt.init) : "";
		char *cond = stmt->for_stmt.cond ? gen_expr_c(stmt->for_stmt.cond) : "";
		char *next = stmt->for_stmt.next ? gen_stmt_c(stmt->for_stmt.next) : "";
		return strf("for (%s; %s; %s) %s", init, cond, next, gen_stmt_block_c(stmt->for_stmt.block));
	}

	case STMT_DO: {
		return strf("do %s while(%s)", 
			gen_stmt_block_c(stmt->while_stmt.block),
			gen_expr_c(stmt->while_stmt.cond));
	}

	case STMT_WHILE: {
		return strf("while(%s) %s", 
			gen_expr_c(stmt->while_stmt.cond),
			gen_stmt_block_c(stmt->while_stmt.block));
	}

	case STMT_SWITCH: {
		BUF(char *str) = NULL; // @LEAK
		da_printf(str, "switch (%s) {", gen_expr_c(stmt->switch_stmt.expr));
		++gen_indent;
		gen_newline(str);
		for (int i=0; i < stmt->switch_stmt.num_cases; ++i) {
			SwitchCase switch_case = stmt->switch_stmt.cases[i];
			if (switch_case.is_default) {
				da_printf(str, "default: ");
			} else {
				for (int j=0; j < switch_case.num_exprs; ++j) {
					da_printf(str, "case %s: ", gen_expr_c(switch_case.exprs[j]));
					if (j < switch_case.num_exprs - 1)
						gen_newline(str);
				}
			}
			da_printf(str, "%s", gen_stmt_block_c(switch_case.block));
			if (i < stmt->switch_stmt.num_cases - 1)
				gen_newline(str);
		}
		--gen_indent;
		gen_newline(str);
		da_printf(str, "}");
		return str;
	}

	default:
		assert(0);
		return NULL;
	}
}

bool semicolon_follows_stmt_c(Stmt *stmt) {
	return stmt->kind == STMT_RETURN   ||
		   stmt->kind == STMT_CONTINUE ||
		   stmt->kind == STMT_BREAK    ||
		   stmt->kind == STMT_DO       ||
		   stmt->kind == STMT_ASSIGN   ||
		   stmt->kind == STMT_INIT     ||
		   stmt->kind == STMT_EXPR;
}

char *gen_stmt_block_c(StmtBlock block) {
	BUF(char *str) = NULL; // @LEAK
	da_printf(str, "{");
	++gen_indent;
	gen_newline(str);
	for (int i=0; i<block.num_stmts; ++i) {
		Stmt *stmt = block.stmts[i];
		da_printf(str, "%s", gen_stmt_c(stmt));
		if (semicolon_follows_stmt_c(stmt))
			da_printf(str, ";");
		if (i < block.num_stmts - 1) 
			gen_newline(str);
	}
	--gen_indent;
	gen_newline(str);
	da_printf(str, "}");
	return str;
}

bool note_is_foreign(Decl *decl) {
	for (int i=0; i<decl->notes.num_notes; ++i) {
		if (decl->notes.notes[i].name == name_foreign)
			return true;
	}
	return false;
}

char *gen_sym_c(Sym *sym) {
	Decl *decl = sym->decl;
	assert(decl);

	// don't generate code for foreign declarations
	if (note_is_foreign(decl)) {
		return NULL;
	}

	switch (decl->kind) {
	case DECL_CONST: {
		return strf("enum { %s = %s };", decl->name, gen_expr_c(decl->const_decl.expr));
	}

	case DECL_VAR: {
		char *type = decl->var.typespec
			? gen_typespec_c(decl->var.typespec, decl->name)
			: gen_type_c(sym->type, decl->name);

		if (decl->var.expr) {
			return strf("%s = %s;", type, gen_expr_c(decl->var.expr));
		} else {
			return strf("%s;", type);
		}
	}

	case DECL_TYPEDEF: {
		return strf("typedef %s;", gen_typespec_c(decl->typedef_decl.typespec, decl->name));
	}

	case DECL_UNION:
	case DECL_STRUCT: {
		BUF(char *str) = NULL; // @LEAK
		da_printf(str, "%s %s {", decl->kind == DECL_STRUCT ? "struct" : "union", decl->name);
		++gen_indent;
		gen_newline(str);

		int num_fields = decl->aggregate.num_fields;
		for (int i=0; i<num_fields; ++i) {
			AggregateField field = decl->aggregate.fields[i];
			da_printf(str, "%s;", gen_typespec_c(field.typespec, field.name));
			if (i < num_fields - 1) 
				gen_newline(str);
		}
		--gen_indent;
		gen_newline(str);
		da_printf(str, "};");
		// TODO(shaw): wasting space in stretchy buf from len to cap
		return str;
	}

	case DECL_ENUM: {
		BUF(char *str) = NULL; // @LEAK
		da_printf(str, "enum %s {", decl->name);
		++gen_indent;
		gen_newline(str);

		int num_items = decl->enum_decl.num_items;
		for (int i=0; i<num_items; ++i){
			EnumItem item = decl->enum_decl.items[i];
			da_printf(str, "%s", item.name);
			if (item.expr)
				da_printf(str, " = %s", gen_expr_c(item.expr));
			da_printf(str, ",");
			if (i < num_items - 1) 
				gen_newline(str);
		}
		--gen_indent;
		gen_newline(str);
		da_printf(str, "};");
		return str;
	}

	case DECL_FUNC: {
		BUF(char *str) = NULL; // @LEAK

		if (decl->func.ret_typespec) {
			da_printf(str, "%s ", gen_typespec_c(decl->func.ret_typespec, ""));
		} else {
			da_printf(str, "void ");
		}

		da_printf(str, "%s(", decl->name);

		int num_params = decl->func.num_params;
		if (num_params == 0) {
			da_printf(str, "void");
		}
		for (int i=0; i<num_params; ++i) {
			FuncParam param = decl->func.params[i];
			da_printf(str, "%s%s", gen_typespec_c(param.typespec, param.name), i == num_params-1 ? "" : ", ");
		}
		if (decl->func.is_variadic) {
			da_printf(str, ", ...");
		}
		da_printf(str, ") %s", gen_stmt_block_c(decl->func.block));

		return str;
	}

	default: 
		assert(0);
		return NULL;
	}
}

char *gen_preamble_c(void) {
	BUF(char *preamble) = NULL;
	// c lib includes
	da_printf(preamble, "%s\n", "#include <stdio.h>\n#include <stdbool.h>\n#include <stdlib.h>\n#include <time.h>\n");
	// typedefs for primative types
	da_printf(preamble, "%s\n", "typedef signed char schar;");
	da_printf(preamble, "%s\n", "typedef unsigned char uchar;");
	da_printf(preamble, "%s\n", "typedef unsigned short ushort;");
	da_printf(preamble, "%s\n", "typedef unsigned int uint;");
	da_printf(preamble, "%s\n", "typedef unsigned long ulong;");
	da_printf(preamble, "%s\n", "typedef long long longlong;");
	da_printf(preamble, "%s\n", "typedef unsigned long long ulonglong;");
	return preamble;
}


int compile_file(char *path);

void codegen_test(void) {
    SourcePos pos = {"", 0};
	char *str;

	// exprs
    str = gen_expr_c(expr_int(pos, 69, TOKENMOD_NONE));
	assert(0 == strcmp(str, "69"));
    str = gen_expr_c(expr_float(pos, 6.66, TOKENMOD_NONE));
	assert(0 == strcmp(str, "6.66"));
    str = gen_expr_c(expr_bool(pos, true));
	assert(0 == strcmp(str, "true"));
    str = gen_expr_c(expr_bool(pos, false));
	assert(0 == strcmp(str, "false"));
    str = gen_expr_c(expr_str(pos, "well hello friends"));
	assert(0 == strcmp(str, "\"well hello friends\""));
    str = gen_expr_c(expr_name(pos, "Vector3"));
	assert(0 == strcmp(str, "Vector3"));
    str = gen_expr_c(expr_unary(pos, '~', expr_int(pos, 42, TOKENMOD_NONE)));
	assert(0 == strcmp(str, "~(42)"));
    str = gen_expr_c(expr_binary(pos, '+', expr_int(pos, 33, TOKENMOD_NONE), expr_int(pos, 36, TOKENMOD_NONE)));
	assert(0 == strcmp(str, "(33) + (36)"));
    str = gen_expr_c(expr_cast(pos, typespec_name(pos, "int"), expr_name(pos, "x")));
	assert(0 == strcmp(str, "(int)(x)"));
    str = gen_expr_c(expr_sizeof_expr(pos, expr_name(pos, "x")));
	assert(0 == strcmp(str, "sizeof(x)"));
    str = gen_expr_c(expr_sizeof_typespec(pos, typespec_name(pos, "int")));
	assert(0 == strcmp(str, "sizeof(int)"));
     
	// types
    str = gen_type_c(type_int, "");
	assert(0 == strcmp(str, "int"));
    str = gen_type_c(type_char, "");
	assert(0 == strcmp(str, "char"));
    str = gen_type_c(type_float, "");
	assert(0 == strcmp(str, "float"));
    str = gen_type_c(type_bool, "");
	assert(0 == strcmp(str, "bool"));

	TypeField params[] = {
		{ "param1", type_int },
		{ "param2", type_int },
	};
	Type *func_int_int = type_func(params, ARRAY_COUNT(params), false, type_int);

	str = gen_type_c(type_array(type_int, 16), "x");
	assert(0 == strcmp(str, "int (x[16])"));
	str = gen_type_c(func_int_int, "x");
	assert(0 == strcmp(str, "int (*x)(int, int)"));
	str = gen_type_c(type_ptr(func_int_int), "x");
	assert(0 == strcmp(str, "int (*(*x))(int, int)"));
	str = gen_type_c(type_array(type_ptr(func_int_int), 3), "x");
	assert(0 == strcmp(str, "int (*(*(x[3])))(int, int)"));
	

	if (compile_file("tests/codegen_test.ion") != 0) {
		fprintf(stderr, "Failed to compile codegen_test.ion\n");
		return;
	}

	map_clear(&global_syms_map);
	da_free(global_syms_buf);
	da_free(ordered_syms);
}
#undef INDENT_WIDTH

