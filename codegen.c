// @LEAK all the strf calls allocate and leak, right now strf calls malloc, but
// will switch to arena allocator in future

typedef struct {
	Stmt **continue_scope;
	Stmt **break_scope;
	Stmt **block_scope;
} DeferScope;

#define MAX_DEFER_STACK 4096
Stmt *defer_stack[MAX_DEFER_STACK];
Stmt **defer_stack_end = defer_stack;
// the innermost scope that break or continue refer to
Stmt **defer_break_scope = defer_stack;
Stmt **defer_continue_scope = defer_stack;

Stmt **defer_enter_scope(void) {
	return defer_stack_end;
}

void defer_leave_scope(Stmt **scope_start, bool leave_continue, bool leave_break, bool leave_block) {
	if (leave_continue)
		defer_continue_scope = scope_start;
	if (leave_break)
		defer_break_scope = scope_start;
	if (leave_block)
		defer_stack_end = scope_start;
}

void defer_push(Stmt *stmt) {
	if (defer_stack_end > defer_stack + MAX_DEFER_STACK) {
		fatal("Too many active defer statements, max is %d", MAX_DEFER_STACK);
	}
	*defer_stack_end++ = stmt;
}

Stmt *defer_pop(Stmt **scope_start) {
	if (!scope_start) scope_start = defer_stack;
	if (defer_stack_end <= scope_start || defer_stack_end == defer_stack) {
		return NULL;
	}
	--defer_stack_end;
	return *defer_stack_end;
}

Map gen_name_map;

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

char *gen_name_c(char *name) {
	char *gen_name = map_get(&gen_name_map, name);
	if (!gen_name) {
		Sym *sym = map_get(&reachable_syms_map, name);
		if (sym) {
			if (sym->external_name) {
				gen_name = sym->external_name;
			} else if (sym->package->external_name) {
				gen_name = strf("%s%s", sym->package->external_name, sym->name);
			} else {
				gen_name = sym->name;
			}
		} else {
			assert(name);
			gen_name = name;
		}
		map_put(&gen_name_map, name, gen_name);
	}
	return gen_name;
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
	case TYPE_LLONG:      return strf("llong%s%s", sep, inner);
	case TYPE_ULLONG:     return strf("ullong%s%s", sep, inner);
	case TYPE_FLOAT:      return strf("float%s%s", sep, inner);
	case TYPE_DOUBLE:     return strf("double%s%s", sep, inner);
	case TYPE_BOOL:       return strf("bool%s%s", sep, inner);

	case TYPE_INCOMPLETE: // NOTE(shaw): this triggers an alarm in my head, but here it is used for opaque pointers
	case TYPE_STRUCT:
	case TYPE_UNION:
	case TYPE_ENUM: {
		return strf("%s%s%s", gen_name_c(type->sym->name), sep, inner);
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
char *gen_expr_c(Expr *expr);

char *gen_expr_compound_c(Expr *expr, bool is_init) {
	BUF(char *str) = NULL; // @LEAK

	if (!is_init) {
		if (expr->compound.typespec) {
			da_printf(str, "(%s)", gen_typespec_c(expr->compound.typespec, ""));
		} else {
			da_printf(str, "(%s)", gen_type_c(expr->type, ""));
		}
	}
	da_printf(str, "{");

	int num_args = expr->compound.num_args;
	CompoundArg *args = expr->compound.args;

	if (expr->compound.is_designated_init) {
		for (int i=0; i<num_args; ++i) {
			da_printf(str, ".%s = %s%s", 
				gen_expr_c(args[i].field_name),
				gen_expr_c(args[i].field_value),
				i == num_args - 1 ? "" : ", ");
		}

	} else {
		for (int i=0; i<num_args; ++i) {
			if (args[i].field_index) {
				da_printf(str, "[%s] = ", gen_expr_c(args[i].field_index));
			}
			da_printf(str, "%s%s", 
				gen_expr_c(args[i].field_value),
				i == num_args - 1 ? "" : ", ");
		}
	}

	da_printf(str, "}");
	return str;
}

char *gen_expr_int_c(Expr *expr) {
	assert(expr->kind == EXPR_INT);

	BUF(char *fmt) = NULL;
	da_printf(fmt, "%%");

	bool is_hex = IS_SET(expr->mod, TOKENMOD_HEX);
	bool is_bin = IS_SET(expr->mod, TOKENMOD_BIN);
	bool is_oct = IS_SET(expr->mod, TOKENMOD_OCT);

	if (is_hex || is_bin || is_oct) {
		da_printf(fmt, "#");
	}

	// add length sub-specifier to format
	if (expr->type == type_ullong || expr->type == type_llong) {
		da_printf(fmt, "l");
	} else if (expr->type == type_ulong || expr->type == type_long) {
		da_printf(fmt, "ll");
	}

	// add specifier to format
	if (is_hex || is_bin) {
		da_printf(fmt, "X");
	} else if (is_oct) {
		da_printf(fmt, "o");
	} else if (is_unsigned_integer_type(expr->type)) {
		da_printf(fmt, "u");
	} else {
		da_printf(fmt, "d");
	}

	// add suffixes
	if (IS_SET(expr->mod, TOKENMOD_UNSIGNED)) {
		da_printf(fmt, "u");
	}
	if (IS_SET(expr->mod, TOKENMOD_LLONG)) {
		da_printf(fmt, "ll");
	} else if (IS_SET(expr->mod, TOKENMOD_LONG)) {
		da_printf(fmt, "l");
	}

	return strf(fmt, expr->int_val);
}

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
		return strf("%s", gen_name_c(expr->name));
    case EXPR_UNARY: 
		return strf("%c(%s)", expr->unary.op, gen_expr_c(expr->unary.expr));
    case EXPR_COMPOUND:
		return gen_expr_compound_c(expr, false);
    case EXPR_INT:
		return gen_expr_int_c(expr);
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
		Expr *call_expr = expr->call.expr;
        char *name = gen_expr_c(call_expr);
        Expr **args = expr->call.args;
        int num_args = expr->call.num_args;
		BUF(char *str) = NULL;

		if (call_expr->type->kind == TYPE_FUNC) {
			// function call
			da_printf(str, "%s(", name);
			for (int i=0; i<num_args; ++i) {
				da_printf(str, "%s%s", gen_expr_c(args[i]), i == num_args - 1 ? "" : ", ");
			}
			da_printf(str, ")");
		} else {
			// cast
			str = strf("(%s)(%s)", name, gen_expr_c(args[0]));
		}
		return str;
    }
    case EXPR_INDEX: 
        return strf("%s[%s]",
			gen_expr_c(expr->index.expr),
            gen_expr_c(expr->index.index));
    case EXPR_FIELD: {
		Type *type = expr->field.expr->type;
		assert(type);
        return strf("%s%s%s",
            gen_expr_c(expr->field.expr),
			type->kind == TYPE_PTR ? "->" : ".",
            expr->field.name);
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
	case TYPESPEC_NAME: {
        return strf("%s%s%s", gen_name_c(typespec->name), sep, inner);
	}

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

char *gen_decl_func_c(Decl *decl) {
	BUF(char *str) = NULL; // @LEAK

	da_printf(str, "%s(", gen_name_c(decl->name));

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
	da_printf(str, ")");

	if (decl->func.ret_typespec) {
		return strf("%s", gen_typespec_c(decl->func.ret_typespec, str));
	} else {
		return strf("void %s", str);
	}

	return str;
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

char *gen_stmt_c(Stmt *stmt, DeferScope defer_scope);
char *gen_stmt_block_c(StmtBlock block, DeferScope defer_Scope);

char *gen_defers_c(DeferScope defer_scope, Stmt **scope_start, bool trailing_newline) {
	BUF(char *str) = NULL;
	Stmt *defer_stmt = defer_pop(scope_start);
	while (defer_stmt) {
		da_printf(str, "%s", gen_stmt_c(defer_stmt, defer_scope));
		if (semicolon_follows_stmt_c(defer_stmt))
			da_printf(str, ";");
		defer_stmt = defer_pop(scope_start);
		if (defer_stmt || trailing_newline)
			gen_newline(str);
	}
	return str;
}


char *gen_stmt_c(Stmt *stmt, DeferScope defer_scope) {
	switch (stmt->kind) {
	case STMT_CONTINUE: {
		char *defers = gen_defers_c(defer_scope, defer_scope.continue_scope, true);
		return defers ? strf("%scontinue", defers) : "continue";
	}
	case STMT_BREAK: {
		char *defers = gen_defers_c(defer_scope, defer_scope.break_scope, true);
		return defers ? strf("%sbreak", defers) : "break";
	}
	case STMT_RETURN: {
		BUF(char *str) = NULL;
		char *defers = gen_defers_c(defer_scope, NULL, true);
		if (defers) {
			da_printf(str, "%s", defers);
		} 
		da_printf(str, "return");
		if (stmt->return_stmt.expr) {
			da_printf(str, " %s", gen_expr_c(stmt->return_stmt.expr));
		}
		return str;
	}
	case STMT_BRACE_BLOCK:
		return gen_stmt_block_c(stmt->block, defer_scope);
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
			char *init_expr = (stmt->init.expr->kind == EXPR_COMPOUND) 
				? gen_expr_compound_c(stmt->init.expr, true)
				: gen_expr_c(stmt->init.expr);
			return strf("%s = %s", type, init_expr);
		} else {
			return strf("%s", type);
		}
	}

	case STMT_IF: {
		BUF(char *str) = NULL;
		da_printf(str, "if (%s) %s", 
				gen_expr_c(stmt->if_stmt.cond),
				gen_stmt_block_c(stmt->if_stmt.then_block, defer_scope));

		ElseIf *else_ifs = stmt->if_stmt.else_ifs; 
		for (int i=0; i < stmt->if_stmt.num_else_ifs; ++i) {
			da_printf(str, " else if (%s) %s", 
				gen_expr_c(else_ifs[i].cond),
				gen_stmt_block_c(else_ifs[i].block, defer_scope));
		}

		StmtBlock else_block = stmt->if_stmt.else_block;
		if (else_block.num_stmts > 0) {
			da_printf(str, " else %s", gen_stmt_block_c(stmt->if_stmt.else_block, defer_scope));
		}
		return str;
	}

	case STMT_FOR: {
		char *init = stmt->for_stmt.init ? gen_stmt_c(stmt->for_stmt.init, defer_scope) : "";
		char *cond = stmt->for_stmt.cond ? gen_expr_c(stmt->for_stmt.cond) : "";
		char *next = stmt->for_stmt.next ? gen_stmt_c(stmt->for_stmt.next, defer_scope) : "";

		Stmt **loop_scope = defer_enter_scope();
		DeferScope new_defer_scope = { 
			.continue_scope = loop_scope,
			.break_scope    = loop_scope,
			.block_scope    = defer_scope.block_scope,
		};
		char *block = gen_stmt_block_c(stmt->for_stmt.block, new_defer_scope);
		defer_leave_scope(loop_scope, true, true, false);

		return strf("for (%s; %s; %s) %s", init, cond, next, block);
	}

	case STMT_DO: {
		Stmt **loop_scope = defer_enter_scope();
		DeferScope new_defer_scope = { 
			.continue_scope = loop_scope,
			.break_scope    = loop_scope,
			.block_scope    = defer_scope.block_scope,
		};
		char *block = gen_stmt_block_c(stmt->while_stmt.block, new_defer_scope);
		defer_leave_scope(loop_scope, true, true, false);

		char *cond = gen_expr_c(stmt->while_stmt.cond);

		return strf("do %s while(%s)", block, cond);
	}

	case STMT_WHILE: {
		Stmt **loop_scope = defer_enter_scope();
		DeferScope new_defer_scope = { 
			.continue_scope = loop_scope,
			.break_scope    = loop_scope,
			.block_scope    = defer_scope.block_scope,
		};
		char *block = gen_stmt_block_c(stmt->while_stmt.block, new_defer_scope);
		defer_leave_scope(loop_scope, true, true, false);

		char *cond = gen_expr_c(stmt->while_stmt.cond);

		return strf("while(%s) %s", cond, block);
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

			Stmt **new_break_scope = defer_enter_scope();
			defer_scope.break_scope = new_break_scope;
			da_printf(str, "%s", gen_stmt_block_c(switch_case.block, defer_scope));
			defer_leave_scope(new_break_scope, false, true, false);

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

char *gen_stmt_block_c(StmtBlock block, DeferScope defer_scope) {
	BUF(char *str) = NULL; // @LEAK
	da_printf(str, "{");
	++gen_indent;
	gen_newline(str);

	Stmt **block_scope = defer_enter_scope();
	defer_scope.block_scope = block_scope;
	for (int i=0; i<block.num_stmts; ++i) {
		Stmt *stmt = block.stmts[i];
		if (stmt->kind == STMT_DEFER) {
			defer_push(stmt->defer.stmt);
			continue;
		}
		da_printf(str, "%s", gen_stmt_c(stmt, defer_scope));
		if (semicolon_follows_stmt_c(stmt))
			da_printf(str, ";");
		if (i < block.num_stmts - 1) 
			gen_newline(str);
	}

	// generate defers for current scope
	char *defers = gen_defers_c(defer_scope, block_scope, false);
	if (defers) {
		gen_newline(str);
		da_printf(str, "%s", defers);
	}
	defer_leave_scope(block_scope, false, false, true);

	--gen_indent;
	gen_newline(str);
	da_printf(str, "}");
	return str;
}

// generate declaration from symbol
char *gen_sym_decl_c(Sym *sym) {
	Decl *decl = sym->decl;
	assert(decl);

	// don't generate code for foreign declarations
	if (is_foreign_decl(decl)) {
		return NULL;
	}

	switch (decl->kind) {
	case DECL_CONST:
		return strf("#define %s (%s)", gen_name_c(decl->name), gen_expr_c(decl->const_decl.expr));
		break;
	case DECL_TYPEDEF: {
		char *decl_name = gen_name_c(decl->name);
		return strf("typedef %s;", gen_typespec_c(decl->typedef_decl.typespec, decl_name));
	}

	case DECL_UNION:
	case DECL_STRUCT: {
		int num_fields = decl->aggregate.num_fields;
		if (num_fields == 0) {
			// NOTE: rely on aggregate declarations all being generated at the top of the file in gen_forward_decls()
			break;
		}

		BUF(char *str) = NULL; // @LEAK
		da_printf(str, "%s %s {", decl->kind == DECL_STRUCT ? "struct" : "union", gen_name_c(decl->name));
		++gen_indent;
		gen_newline(str);

		for (int i=0; i<num_fields; ++i) {
			AggregateField field = decl->aggregate.fields[i];
			da_printf(str, "%s;", gen_typespec_c(field.typespec, field.name));
			if (i < num_fields - 1) 
				gen_newline(str);
		}
		--gen_indent;
		gen_newline(str);
		da_printf(str, "};");
		return str;
	}

	case DECL_ENUM: {
		BUF(char *str) = NULL; // @LEAK
		da_printf(str, "enum "); 
		if (!decl->enum_decl.is_anonymous) {
			da_printf(str, "%s ", gen_name_c(decl->name));
		}
		da_printf(str, "{"); 
		++gen_indent;
		gen_newline(str);

		int num_items = decl->enum_decl.num_items;
		for (int i=0; i<num_items; ++i){
			EnumItem item = decl->enum_decl.items[i];
			da_printf(str, "%s", gen_name_c(item.name));
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

	case DECL_VAR: {
		char *decl_name = gen_name_c(decl->name);
		char *str = decl->var.typespec
			? gen_typespec_c(decl->var.typespec, decl_name)
			: gen_type_c(sym->type, decl_name);
		return strf("extern %s;", str); 
	}

	case DECL_FUNC: {
		return strf("%s;", gen_decl_func_c(decl));
	}
	
	default: 
		assert(0);
		return NULL;
	}

	return NULL;
}

// generate definition from symbol
char *gen_sym_def_c(Sym *sym) {
	Decl *decl = sym->decl;
	assert(decl);

	// don't generate code for foreign declarations
	if (is_foreign_decl(decl)) {
		return NULL;
	}

	switch (decl->kind) {
	case DECL_ENUM:
	case DECL_UNION:
	case DECL_STRUCT:
	case DECL_TYPEDEF:
		// these are declaration only, so do nothing here
		break;
	case DECL_CONST:
		// constants must be defined with its declaration for dependancies to work properly
		// meaning in gen_sym_decl_c(), so nothing is done here
		break;	

	case DECL_VAR: {
		char *decl_name = gen_name_c(decl->name);
		char *type = decl->var.typespec
		   ? gen_typespec_c(decl->var.typespec, decl_name)
		   : gen_type_c(sym->type, decl_name);

		if (decl->var.expr) {
			char *init_expr = (decl->var.expr->kind == EXPR_COMPOUND) 
				? gen_expr_compound_c(decl->var.expr, true)
				: gen_expr_c(decl->var.expr);
			return strf("%s = %s;", type, init_expr);
		} else { 
			return strf("%s;", type);
		}
	}

	case DECL_FUNC: {
		if (!decl->func.is_incomplete) {
			char *signature = gen_decl_func_c(decl);
			DeferScope defer_scope = {0};
			return strf("%s %s", signature, gen_stmt_block_c(decl->func.block, defer_scope));
		} 
		break;
	}
	
	default:
		assert(0);
		break;
	}

	return NULL;
}


char *gen_preamble_c(void) {
	BUF(char *preamble) = NULL;
	da_printf(preamble, "// Preamble -------------------------------------------------------------------\n");
	// c lib includes
	da_printf(preamble, "%s\n", "#include <stdbool.h>\n");
	// typedefs for primative types
	da_printf(preamble, "%s\n", "typedef signed char schar;");
	da_printf(preamble, "%s\n", "typedef unsigned char uchar;");
	da_printf(preamble, "%s\n", "typedef unsigned short ushort;");
	da_printf(preamble, "%s\n", "typedef unsigned int uint;");
	da_printf(preamble, "%s\n", "typedef unsigned long ulong;");
	da_printf(preamble, "%s\n", "typedef long long llong;");
	da_printf(preamble, "%s\n", "typedef unsigned long long ullong;");
	return preamble;
}
	
char *gen_foreign_headers_c(void) {
	BUF(char *headers) = NULL;
	da_printf(headers, "// Foreign headers ------------------------------------------------------------\n");
	// directive headers
	for (int j=0; j<da_len(packages); ++j) {
		Package *package = packages[j];
		for (int i=0; i<da_len(package->directives); ++i) {
			Decl *decl = package->directives[i];
			if (decl->name == name_foreign) {
				for (int j=0; j<decl->directive.foreign.num_args; ++j) {
					NoteArg arg = decl->directive.foreign.args[j];
					if (arg.name == name_header) {
						char *val = arg.expr->str_val;
						if (val[0] == '<') {
							// system include
							da_printf(headers, "#include %s\n", val);
						} else {
							// local include
							da_printf(headers, "#include \"%s\"\n", val);
						}
					}
				}
			}
		}
	}
	return headers;
}


char *gen_forward_decls_c(void) {
	BUF(char *str) = NULL;

	// forward declare types
	da_printf(str, "// Forward declared types -----------------------------------------------------");
	gen_newline(str);
	for (int i=0; i<da_len(reachable_syms); ++i) {
		Sym *sym = reachable_syms[i];
		Decl *decl = sym->decl;
		// NOTE(shaw): primative types don't have declarations, skip those
		if (!decl) continue;
		if (is_foreign_decl(decl)) continue;
		if (sym->kind == SYM_TYPE) {
			char *name = gen_name_c(sym->name);
			if (decl->kind == DECL_STRUCT) {
				da_printf(str, "typedef struct %s %s;", name, name);
				gen_newline(str);
			} else if (decl->kind == DECL_UNION) {
				da_printf(str, "typedef union %s %s;", name, name);
				gen_newline(str);
			} else if (decl->kind == DECL_ENUM) {
				if (!decl->enum_decl.is_anonymous) {
					da_printf(str, "typedef enum %s %s;", name, name);
					gen_newline(str);
				}
			}
		} 
	}
	return str;
}

char *gen_decls_c(void) {
	BUF(char *cdecls) = NULL;
	da_printf(cdecls, "// Declarations ---------------------------------------------------------------");
	gen_newline(cdecls);
	for (int i = 0; i<da_len(ordered_syms); ++i) {
		char *decl_str = gen_sym_decl_c(ordered_syms[i]);
		if (decl_str) {
			da_printf(cdecls, "%s", decl_str);
			gen_newline(cdecls);
		}
	}
	return cdecls;
}

char *gen_definitions_c(void) {
	BUF(char *defs) = NULL;
	da_printf(defs, "// Definitions ----------------------------------------------------------------");
	gen_newline(defs);
	for (int i = 0; i<da_len(ordered_syms); ++i) {
		char *def_str = gen_sym_def_c(ordered_syms[i]);
		if (def_str) {
			da_printf(defs, "%s", def_str);
			gen_newline(defs);
		}
	}
	return defs;
}

char *gen_foreign_sources_c(void) {
	BUF(char *sources) = NULL;
	da_printf(sources, "// Foreign sources ------------------------------------------------------------\n");
	for (int j=0; j<da_len(packages); ++j) {
		Package *package = packages[j];
		for (int i=0; i<da_len(package->directives); ++i) {
			Decl *decl = package->directives[i];
			if (decl->name == name_foreign) {
				for (int j=0; j<decl->directive.foreign.num_args; ++j) {
					NoteArg arg = decl->directive.foreign.args[j];
					if (arg.name == name_source) {
						char source_path[MAX_PATH];
						path_copy(source_path, package->full_path);
						path_join(source_path, arg.expr->str_val);
						da_printf(sources, "#include \"%s\"\n", source_path);
					}
				}
			}
		}
	}
	return sources;
}


void gen_all_c(FILE *out_file) {
	assert(out_file);
	char *preamble = gen_preamble_c();
	char *headers = gen_foreign_headers_c();
	char *forward_decls = gen_forward_decls_c();
	char *cdecls = gen_decls_c();
	char *defs = gen_definitions_c();
	char *sources = gen_foreign_sources_c();
	fprintf(out_file, "%s\n%s\n%s\n%s\n%s\n%s", preamble, headers, forward_decls, cdecls, defs, sources);
}

bool compile_package(char *package_name, char *out_name);

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
	

	// if (!compile_package("tests/codegen")) {
		// fprintf(stderr, "Failed to compile tests/codegen\n");
		// return;
	// }

	// map_clear(&global_syms_map);
	// da_free(global_syms_buf);
	// da_free(ordered_syms);
}
#undef INDENT_WIDTH

