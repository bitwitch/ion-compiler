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
    case EXPR_COMPOUND: {
        // TODO(shaw): get the decl for the type here so that we can have named fields (designated initializer)
        // (Vec2){ .x = 69, .y = 420 }

		// TODO(shaw): handle more than TYPESPEC_NAME
		assert(expr->compound.type->kind == TYPESPEC_NAME);
        int num_args = expr->compound.num_args;
        char *str = strf("(%s){", expr->compound.type->name);
        for (int i=0; i<num_args; ++i) {
            str = strf("%s%s%s",
                str,
                gen_expr_c(expr->compound.args[i]),
                i == num_args - 1 ? "" : ", ");
        }
        return strf("%s}", str);
    }
	case EXPR_CAST:
		// TODO(shaw): handle more than TYPESPEC_NAME
		assert(expr->cast.type->kind == TYPESPEC_NAME);
		return strf("(%s)(%s)", 
			expr->cast.type->name, 
			gen_expr_c(expr->cast.expr));
	case EXPR_SIZEOF_EXPR:
        return strf("sizeof(%s)", gen_expr_c(expr->sizeof_expr));
    case EXPR_SIZEOF_TYPE:
		// TODO(shaw): handle more than TYPESPEC_NAME
		assert(expr->sizeof_type->kind == TYPESPEC_NAME);
        return strf("sizeof(%s)", expr->sizeof_type->name);
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

	case DECL_TYPEDEF: {
		// TODO(shaw): handle more than TYPESPEC_NAME
		assert(decl->typedef_decl.type->kind == TYPESPEC_NAME);
		return strf("typedef %s %s;", decl->typedef_decl.type->name, decl->name);
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
			// TODO(shaw): handle more than TYPESPEC_NAME
			assert(field.type->kind == TYPESPEC_NAME);
			Sym *field_sym = sym_get(field.type->name);
			da_printf(str, "%s;", gen_type_c(field_sym->type, field.name));
			if (i == num_fields - 1) 
				--gen_indent;
			gen_newline(str);
		}
		da_printf(str, "}");
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
			if (i == num_items - 1) 
				--gen_indent;
			gen_newline(str);
		}
		da_printf(str, "}");
		return str;
	}

	case DECL_FUNC:
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
	Expr *args[] = { expr_int(pos, 1), expr_int(pos, 2), expr_int(pos, 3) };
    str = gen_expr_c(expr_compound(pos, typespec_name(pos, "x"), args, array_count(args)));
	assert(0 == strcmp(str, "(x){1, 2, 3}"));
    str = gen_expr_c(expr_cast(pos, typespec_name(pos, "int"), expr_name(pos, "x")));
	assert(0 == strcmp(str, "(int)(x)"));
    str = gen_expr_c(expr_sizeof_expr(pos, expr_name(pos, "x")));
	assert(0 == strcmp(str, "sizeof(x)"));
    str = gen_expr_c(expr_sizeof_type(pos, typespec_name(pos, "int")));
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
	Type *func_int_int = type_func(params, array_count(params), type_int);

	str = gen_type_c(type_array(type_int, 16), "x");
	assert(0 == strcmp(str, "int (x)[16]"));
	str = gen_type_c(func_int_int, "x");
	assert(0 == strcmp(str, "int (x)(int, int)"));
	str = gen_type_c(type_ptr(func_int_int), "x");
	assert(0 == strcmp(str, "int (*(x))(int, int)"));
	str = gen_type_c(type_array(type_ptr(func_int_int), 3), "x");
	assert(0 == strcmp(str, "int (*((x)[3]))(int, int)"));
	

	if (compile_file("codegen_test.ion") != 0) {
		printf("Compilation failed\n");
		return;
	}

	for (int i = 0; i<da_len(ordered_syms); ++i) {
		Sym *sym = ordered_syms[i];
		char *str = gen_sym_c(ordered_syms[i]);
		printf("%s\n", str);
	}
}
#undef INDENT_WIDTH
