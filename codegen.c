// @LEAK all the strf calls allocate and leak, right now strf calls malloc, but
// will switch to arena allocator in future

char *gen_type_c(Type *type, char *inner) {
	assert(type);
	char *sep = *inner ? " " : "";
	switch(type->kind) {
	case TYPE_VOID:
		return strf("void%s%s", sep, inner);
	case TYPE_INT:
		return strf("int%s%s", sep, inner);
	case TYPE_CHAR:
		return strf("char%s%s", sep, inner);
	case TYPE_FLOAT:
		return strf("float%s%s", sep, inner);
	case TYPE_BOOL:
		return strf("bool%s%s", sep, inner);

	case TYPE_PTR: {
		// int *(x)
		char *str = strf("*(%s)", inner);
		return gen_type_c(type->ptr.base, str);
	}

	case TYPE_ARRAY: {
		// int (x)[8]
		char *str = strf("(%s)[%d]", inner, type->array.num_items);
		return gen_type_c(type->array.base, str);
	}

	case TYPE_FUNC: {
		// int (x)(int)
		BUF(char *params) = NULL; // @LEAK
		int num_params = type->func.num_params;
		for (int i=0; i<num_params; ++i) {
			TypeField param = type->func.params[i];
			da_printf(params, "%s%s", gen_type_c(param.type, ""), i == num_params-1 ? "" : ", ");
		}
		char *str = strf("(%s)(%s)", inner, params);
		return gen_type_c(type->func.ret, str);
	}

	case TYPE_STRUCT:
	case TYPE_UNION:
	case TYPE_ENUM:
	case TYPE_CONST:
	default: 
		assert(0);
		return NULL;
	}

}

char *gen_expr_c(Expr *expr) {
    assert(expr);
    switch (expr->kind) {
    case EXPR_INT: 
        return strf("%d", expr->int_val);
    case EXPR_FLOAT: 
        return strf("%g", expr->float_val);
	case EXPR_BOOL:
		return strf("%s", expr->bool_val ? "true" : "false"); 
    case EXPR_STR: 
        return strf("\"%s\"", expr->str_val); 
    case EXPR_NAME: 
        return strf("%s", expr->name); 
    case EXPR_UNARY: 
		return strf("%c%s", expr->unary.op, gen_expr_c(expr->unary.expr));
    case EXPR_BINARY:
        return strf("%s %s %s", 
            gen_expr_c(expr->binary.left),
            token_kind_to_str(expr->binary.op),
            gen_expr_c(expr->binary.right));
    case EXPR_TERNARY: 
        return strf("%s ? %s : %s",
            gen_expr_c(expr->ternary.cond),
            gen_expr_c(expr->ternary.then_expr),
            gen_expr_c(expr->ternary.else_expr));
    case EXPR_CALL: {
        char *name = expr->call.expr->name;
        Expr **args = expr->call.args;
        int num_args = expr->call.num_args;

        char *str = strf("%s(", name);
        for (int i=0; i<num_args; ++i) {
            str = strf("%s%s%s",
                str,
                gen_expr_c(args[i]),
                i == num_args - 1 ? "" : ", ");
        }
        return strf("%s)", str);
    }
    case EXPR_INDEX: 
        return strf("%s[%s]",
			gen_expr_c(expr->index.expr),
            gen_expr_c(expr->index.index));
    case EXPR_FIELD: 
        return strf("%s.%s",
            gen_expr_c(expr->field.expr),
            expr->field.name);
	/*
    case EXPR_COMPOUND: {
        // TODO(shaw): get the decl for the type here so that we can have named fields (designated initializer)
        // (Vec2){ .x = 69, .y = 420 }
        int num_args = expr->compound.num_args;
        char *str = strf("(%s){", gen_type_c(expr->compound.type));
        for (int i=0; i<num_args; ++i) {
            str = strf("%s%s%s",
                str,
                gen_expr_c(expr->compound.args[i]),
                i == num_args - 1 ? "" : ", ");
        }
        return strf("%s}", str):
    }
	case EXPR_CAST:
		return strf("(%s)(%s)", 
			gen_type_c(expr->cast.type), 
			gen_expr_c(expr->cast.expr));
	case EXPR_SIZEOF_EXPR:
        return strf("sizeof(%s)", gen_expr_c(expr->sizeof_expr));
    case EXPR_SIZEOF_TYPE:
        return strf("sizeof(%s)", gen_type_c(expr->sizeof_type));
	*/
    default:
        printf("Error: Codegen: Unknown expr kind: %d\n", expr->kind);
        assert(0);
        return NULL;
    }
}

char *gen_sym_c(Sym *sym) {
	Decl *decl = sym->decl;
	assert(decl);

	switch (decl->kind) {
	case DECL_VAR: {
		char *str = strf("%s", gen_type_c(sym->type, sym->name));
		if (decl->var.expr)
			str = strf("%s = %s", str, gen_expr_c(decl->var.expr));
		return strf("%s;", str);
	}

	case DECL_FUNC:
	case DECL_ENUM:
	case DECL_STRUCT:
	case DECL_UNION:
	case DECL_TYPEDEF:
	case DECL_CONST:
	default: 
		assert(0);
		return NULL;
	}
}


void codegen_test(void) {
    SourcePos pos = {"", 0};
	char *str;

	// exprs
    str = gen_expr_c(expr_int(pos, 69));
	assert(0 == strcmp(str, "69"));
    str = gen_expr_c(expr_float(pos, 6.66));
	assert(0 == strcmp(str, "6.66"));
    str = gen_expr_c(expr_bool(pos, true));
	assert(0 == strcmp(str, "true"));
    str = gen_expr_c(expr_bool(pos, false));
	assert(0 == strcmp(str, "false"));
    str = gen_expr_c(expr_str(pos, "well hello friends"));
	assert(0 == strcmp(str, "\"well hello friends\""));
    str = gen_expr_c(expr_name(pos, "Vector3"));
	assert(0 == strcmp(str, "Vector3"));
    str = gen_expr_c(expr_unary(pos, '~', expr_int(pos, 42)));
	assert(0 == strcmp(str, "~42"));
    str = gen_expr_c(expr_binary(pos, '+', expr_int(pos, 33), expr_int(pos, 36)));
	assert(0 == strcmp(str, "33 + 36"));
     
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
	Type *func_int_int = type_func(params, 2, type_int);

	str = gen_type_c(type_array(type_int, 16), "x");
	assert(0 == strcmp(str, "int (x)[16]"));
	str = gen_type_c(func_int_int, "x");
	assert(0 == strcmp(str, "int (x)(int, int)"));
	str = gen_type_c(type_ptr(func_int_int), "x");
	assert(0 == strcmp(str, "int (*(x))(int, int)"));
	str = gen_type_c(type_array(type_ptr(func_int_int), 3), "x");
	assert(0 == strcmp(str, "int (*((x)[3]))(int, int)"));
	
	/*
	if (compile_file("codegen_test.ion") != 0) {
		printf("Compilation failed\n");
		return;
	}

	for (int i = 0; i<da_len(ordered_syms); ++i) {
		Sym *sym = ordered_syms[i];
		char *str = gen_sym_c(ordered_syms[i]);
		printf("%s\n", str);
	}

	*/
}
