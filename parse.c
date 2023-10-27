Expr *parse_expr(void);
Stmt *parse_stmt(void);
Typespec *parse_typespec(void);
FuncParam parse_decl_func_param(void);

char *parse_name(void) {
    if (is_token(TOKEN_NAME)) {
        char *name = token.name;
        next_token();
        return name;
    } else {
        syntax_error("Expected name, got %s", token_kind_to_str(token.kind));
        return NULL;
    }
}

Typespec *parse_typespec_func_param(void) {
	Token lookahead = lookahead_token();
	if (lookahead.kind == ':') {
		// skip over parameter name
		parse_name();
		expect_token(':');
	}
	return parse_typespec();
}

Typespec *parse_typespec_func(void) {
	SourcePos pos = token.pos;
	bool is_variadic = false;
	expect_token('(');
	BUF(Typespec **param_typespecs) = NULL; // @LEAK

	if (!is_token(')')) {
		// parse first parameter
		if (match_token(TOKEN_ELLIPSIS)) {
			syntax_error("at least one parameter must precede '...' in a variadic function");
			is_variadic = true;
		} else { 
			da_push(param_typespecs, parse_typespec_func_param());
		}
		// parse remaining parameters
		while (match_token(',')) {
			if (match_token(TOKEN_ELLIPSIS)) {
				is_variadic = true;
			} else {
				if (is_variadic) syntax_error("no parameters can follow '...' in a variadic function");
				da_push(param_typespecs, parse_typespec_func_param());
			}
		}
	}
	expect_token(')');

	Typespec *ret_type = NULL;
	if (match_token(':')) {
		ret_type = parse_typespec();
	}

	return typespec_func(pos, param_typespecs, da_len(param_typespecs), is_variadic, ret_type);
}

Typespec *parse_typespec_base(void) {
	if (is_token(TOKEN_NAME)) {
		SourcePos pos = token.pos;
		char *name = parse_name();
		return typespec_name(pos, name);
	} else if (match_keyword(keyword_func)) {
		return parse_typespec_func();
	} else {
        syntax_error("Unexpected token in type: %s", token_info());
        return NULL;
    }
}

Typespec *parse_typespec(void) {
	SourcePos pos = token.pos;
    Typespec *typespec = parse_typespec_base();
    while (is_token('[') || is_token('*')) {
        if (match_token('[')) {
            Expr *size = is_token(']') ? NULL : parse_expr();
            expect_token(']');
            typespec = typespec_array(pos, typespec, size);
        } else {
            assert(is_token('*'));
            next_token();
            typespec = typespec_ptr(pos, typespec);
        }
    }
    return typespec;
}

Expr *parse_expr_compound(Typespec *typespec) {
	SourcePos pos = token.pos;
    expect_token('{');
    BUF(CompoundArg *args) = NULL; // @LEAK
    do {
		CompoundArg arg = {0};
		Expr *expr = parse_expr();
		if (match_token('=')) {
			arg.field_name = expr;
			arg.field_value = parse_expr();
		} else {
			arg.field_value = expr;
		}
		da_push(args, arg);
    } while (match_token(','));
    expect_token('}');
    return expr_compound(pos, typespec, args, da_len(args));
}

Expr *parse_expr_base(void) {
	SourcePos pos = token.pos;
    Expr *expr = NULL;
    if (is_token(TOKEN_INT)) {
        expr = expr_int(pos, token.int_val, token.mod);
        next_token();
    } else if (is_token(TOKEN_FLOAT)) {
		expr = expr_float(pos, token.float_val, token.mod);
        next_token();
    } else if (is_token(TOKEN_STR)) {
		expr = expr_str(pos, token.str_val);
        next_token();
    } else if (is_token(TOKEN_CHAR)) {
		expr = expr_char(pos, token.int_val);
        next_token();
	} else if (is_token(TOKEN_NAME)) {
		char *name = token.name;
		next_token();
		if (is_token('{')) {
			Typespec *typespec = typespec_name(pos, name);
			expr = parse_expr_compound(typespec);
		} else {
			expr = expr_name(pos, name);
		}
    } else if (match_keyword(keyword_cast)) {
        expect_token('(');
        Typespec *typespec = parse_typespec();
        expect_token(',');
        Expr *sub_expr = parse_expr();
        expect_token(')');
		expr = expr_cast(pos, typespec, sub_expr);
    } else if (match_keyword(keyword_sizeof)) {
        expect_token('(');
        if (match_token(':'))
			expr = expr_sizeof_typespec(pos, parse_typespec());
        else
			expr = expr_sizeof_expr(pos, parse_expr());
        expect_token(')');
    } else if (match_token('(')) {
        // compound literal
        if (match_token(':')) {
            Typespec *typespec = parse_typespec();
            expect_token(')');
            expr = parse_expr_compound(typespec);
        } else {
            expr = parse_expr();
            expect_token(')');
        }

    } else if (is_token('{')) {
        expr = parse_expr_compound(NULL);
    } else {
        syntax_error("Unexpected token in base expression: '%s'", token_kind_to_str(token.kind));
    }

    return expr;
}

Expr *parse_expr_call(void) {
	SourcePos pos = token.pos;
    Expr *expr = parse_expr_base();

    while (is_token('(') || is_token('[') || is_token('.')) {
        if (match_token('(')) {
            BUF(Expr **args) = NULL; // @LEAK
            if (!is_token(')')) {
                do {
                    da_push(args, parse_expr());
                } while (match_token(','));
            }
            expect_token(')');
			expr = expr_call(pos, expr, args, da_len(args));
        } else if (match_token('[')) {
			Expr *index = parse_expr();
            expect_token(']');
			expr = expr_index(pos, expr, index);
        } else {
            assert(is_token('.'));
            next_token();
            char *field = token.name;
            expect_token(TOKEN_NAME);
			expr = expr_field(pos, expr, field);
        }
    }
    return expr;
}

Expr *parse_expr_unary(void) {
	SourcePos pos = token.pos;
    if (is_token('+') || is_token('-') || is_token('~') || is_token('&') || is_token('*') || is_token('!')) {
        TokenKind op = token.kind;
        next_token();
		return expr_unary(pos, op, parse_expr_unary());
    }
    return parse_expr_call();
}

Expr *parse_expr_mul(void) {
	SourcePos pos = token.pos;
    Expr *expr = parse_expr_unary();
    
    while (is_token('*') || is_token('/') || is_token('%') || 
           is_token('&') || is_token(TOKEN_LSHIFT) || is_token(TOKEN_RSHIFT))
    {
        TokenKind op = token.kind;
        next_token();
		expr = expr_binary(pos, op, expr, parse_expr_unary());
    }

    return expr;
}

Expr *parse_expr_add(void) {
	SourcePos pos = token.pos;
    Expr *expr = parse_expr_mul();

    while (is_token('+') || is_token('-') || is_token('^') || is_token('|')) {
        TokenKind op = token.kind;
        next_token();
		expr = expr_binary(pos, op, expr, parse_expr_mul());
    }

    return expr;
}

Expr *parse_expr_cmp(void) {
	SourcePos pos = token.pos;
    Expr *expr = parse_expr_add();
    while (is_token_cmp()) {
        TokenKind op = token.kind;
        next_token();
		expr = expr_binary(pos, op, expr, parse_expr_add());
    }
    return expr;
}

Expr *parse_expr_and(void) {
	SourcePos pos = token.pos;
    Expr *expr = parse_expr_cmp();
    while (match_token(TOKEN_LOGICAL_AND)) {
		expr = expr_binary(pos, TOKEN_LOGICAL_AND, expr, parse_expr_cmp());
    }
    return expr;
}

Expr *parse_expr_or(void) {
	SourcePos pos = token.pos;
    Expr *expr = parse_expr_and();
    while (match_token(TOKEN_LOGICAL_OR)) {
		expr = expr_binary(pos, TOKEN_LOGICAL_OR, expr, parse_expr_and());
    }
    return expr;
}

Expr *parse_expr_ternary(void) {
	SourcePos pos = token.pos;
    Expr *expr = parse_expr_or();
    if (match_token('?')) {
        Expr *then_expr = parse_expr_ternary();
        expect_token(':');
        Expr *else_expr = parse_expr_ternary();
		expr = expr_ternary(pos, expr, then_expr, else_expr);
    }
    return expr;
}

Expr *parse_expr(void) {
    return parse_expr_ternary();
}

Expr *parse_expression(char *source) {
    stream = source;
    next_token();
    return parse_expr();
}



StmtBlock parse_stmt_block(void) {
    BUF(Stmt **stmts) = NULL; // @LEAK
    expect_token('{');
    while (!is_token('}'))
        da_push(stmts, parse_stmt());
    expect_token('}');
    return stmt_block(stmts, da_len(stmts));
}

Stmt *parse_stmt_return(void) {
	SourcePos pos = token.pos;
    Expr *expr = NULL;
    if (!is_token(';'))
        expr = parse_expr();
    expect_token(';');
    return stmt_return(pos, expr);
}

Stmt *parse_stmt_defer(void) {
	SourcePos pos = token.pos;
    Stmt *stmt = parse_stmt();
    return stmt_defer(pos, stmt);
}

Stmt *parse_stmt_if(void) {
	SourcePos pos = token.pos;
    expect_token('(');
    Expr *cond = parse_expr();
    expect_token(')');

    StmtBlock then_block = parse_stmt_block();

    BUF(ElseIf *else_ifs) = NULL; // @LEAK
    bool else_block_exists = false;
    while (true) {
        if (!match_keyword(keyword_else)) break;

        if (!match_keyword(keyword_if)) {
            else_block_exists = true;
            break;
        }

        expect_token('(');
        Expr *cond = parse_expr();
        expect_token(')');
        StmtBlock block = parse_stmt_block();
        da_push(else_ifs, (ElseIf){cond, block});
    }

    StmtBlock else_block = {0};
    if (else_block_exists)
        else_block = parse_stmt_block();

	return stmt_if(pos, cond, then_block, else_ifs, da_len(else_ifs), else_block);
}

bool is_assign_op(void) {
    return token.kind == '='             ||
           token.kind == TOKEN_ADD_EQ    ||
           token.kind == TOKEN_SUB_EQ    ||
           token.kind == TOKEN_MUL_EQ    ||
           token.kind == TOKEN_DIV_EQ    ||
           token.kind == TOKEN_MOD_EQ    ||
           token.kind == TOKEN_AND_EQ    ||
           token.kind == TOKEN_OR_EQ     ||
           token.kind == TOKEN_XOR_EQ    ||
           token.kind == TOKEN_LSHIFT_EQ ||
           token.kind == TOKEN_RSHIFT_EQ;
}


Stmt *parse_simple_stmt(void) {
	SourcePos pos = token.pos;
    Expr *expr = parse_expr();
    Stmt *stmt = NULL;

	if (match_token(TOKEN_AUTO_ASSIGN)) {
		if (expr->kind != EXPR_NAME) {
			syntax_error("':=' must be preceded by a name");
			return NULL;
		}
		stmt = stmt_init(pos, expr->name, NULL, parse_expr());
	} else if (match_token(':')) {
		if (expr->kind != EXPR_NAME) {
			syntax_error("':' must be preceded by a name in a variable init statement");
			return NULL;
		}
		Typespec *typespec = parse_typespec();
		Expr *rhs = NULL;
		if (match_token('='))
			rhs = parse_expr();
		stmt = stmt_init(pos, expr->name, typespec, rhs);
    } else if (is_assign_op()) {
        TokenKind op = token.kind;
        next_token();
		stmt = stmt_assign(pos, op, expr, parse_expr());
    } else if (match_token(TOKEN_INC)) {
		stmt = stmt_assign(pos, TOKEN_INC, expr, NULL);
    } else if (match_token(TOKEN_DEC)) {
		stmt = stmt_assign(pos, TOKEN_DEC, expr, NULL);
    } else {
		stmt = stmt_expr(pos, expr);
    }

    return stmt;
}


Stmt *parse_stmt_for(void) {
	SourcePos pos = token.pos;
    expect_token('(');
    Stmt *init = is_token(';') ? NULL : parse_simple_stmt();
    expect_token(';');
    Expr *cond = is_token(';') ? NULL : parse_expr();
    expect_token(';');
    Stmt *next = is_token(')') ? NULL : parse_simple_stmt();
    expect_token(')');
    StmtBlock block = parse_stmt_block();
	return stmt_for(pos, init, cond, next, block);
}

Stmt *parse_stmt_do(void) {
	SourcePos pos = token.pos;
    StmtBlock block = parse_stmt_block();
    if (!match_keyword(keyword_while)) {
        syntax_error("Parsing do-while statement expected 'while', but got '%s'", token_kind_to_str(token.kind));
        return NULL;
    }
    expect_token('(');
    Expr *cond = parse_expr();
    expect_token(')');
    expect_token(';');
	return stmt_do(pos, cond, block);
}

Stmt *parse_stmt_while(void) {
	SourcePos pos = token.pos;
    expect_token('(');
    Expr *cond = parse_expr();
    expect_token(')');
    StmtBlock block = parse_stmt_block();
	return stmt_while(pos, cond, block);
}

SwitchCase parse_stmt_switch_case(void) {
	// SourcePos pos = token.pos;
    BUF(Expr **exprs) = NULL; // @LEAK
    bool is_default = false;
    while (is_keyword(keyword_case) || is_keyword(keyword_default)) {
        if (match_keyword(keyword_case)) {
            da_push(exprs, parse_expr());
        } else {
            is_default = true;
            assert(is_keyword(keyword_default));
            next_token();
        }
		if (!match_token(',')) {
			expect_token(':');
		}
    }

    BUF(Stmt **stmts) = NULL; // @LEAK
    while (!is_token(TOKEN_EOF) && !is_token('}') && !is_keyword(keyword_case) && !is_keyword(keyword_default))
        da_push(stmts, parse_stmt());
	da_push(stmts, stmt_alloc(STMT_BREAK, token.pos)); // implicit break
	return switch_case(exprs, da_len(exprs), is_default, stmt_block(stmts, da_len(stmts)));
}

Stmt *parse_stmt_switch(void) {
	SourcePos pos = token.pos;
    expect_token('(');
    Expr *expr = parse_expr();
    expect_token(')');

    expect_token('{');
    BUF(SwitchCase *cases) = NULL; // @LEAK
    while (!is_token('}'))
        da_push(cases, parse_stmt_switch_case());
    expect_token('}');

	return stmt_switch(pos, expr, cases, da_len(cases));
}

Stmt *parse_stmt_single_token(StmtKind kind) {
    Stmt *stmt = stmt_alloc(kind, token.pos);
    expect_token(';');
    return stmt;
}

Stmt *parse_stmt(void) {
    if (match_keyword(keyword_return))
        return parse_stmt_return();
    else if (match_keyword(keyword_continue))
        return parse_stmt_single_token(STMT_CONTINUE);
    else if (match_keyword(keyword_break))
        return parse_stmt_single_token(STMT_BREAK);
    else if (match_keyword(keyword_if))
        return parse_stmt_if();
    else if (match_keyword(keyword_for))
        return parse_stmt_for();
    else if (match_keyword(keyword_do))
        return parse_stmt_do();
    else if (match_keyword(keyword_while))
        return parse_stmt_while();
    else if (match_keyword(keyword_switch))
        return parse_stmt_switch();
    else if (match_keyword(keyword_defer))
        return parse_stmt_defer();
    else if (is_token('{'))
		return stmt_brace_block(token.pos, parse_stmt_block());
    else {
        Stmt *stmt = parse_simple_stmt();
        expect_token(';');
        return stmt;
    }
}


Decl *parse_decl_enum(void) {
	SourcePos pos = token.pos;
    char *name = parse_name();
    expect_token('{');

    BUF(EnumItem *enum_items) = NULL; // @LEAK

    while(!is_token('}')) {
        char *name = parse_name();
        Expr *expr = NULL;

        if (match_token('='))
            expr = parse_expr();

        if (!is_token('}'))
            expect_token(',');

        da_push(enum_items, (EnumItem){name, expr});
    }
    expect_token('}');
	return decl_enum(pos, name, enum_items, da_len(enum_items));
}

Decl *parse_decl_const(void) {
	SourcePos pos = token.pos;
    char *name = parse_name();
    expect_token('=');
    Expr *expr = parse_expr();
	expect_token(';');
	return decl_const(pos, name, expr);
}

Decl *parse_decl_var(void) {
	SourcePos pos = token.pos;
    char *name = parse_name();
    Typespec *typespec = NULL;
    Expr *expr = NULL;
    if (match_token(':')) {
        typespec = parse_typespec();
    }
    if (match_token('=')) {
        expr = parse_expr();
    }
	expect_token(';');
	return decl_var(pos, name, typespec, expr);
}

Decl *parse_decl_typedef(void) {
	SourcePos pos = token.pos;
    char *name = parse_name();
    expect_token('=');
    Typespec *typespec = parse_typespec();
	expect_token(';');
	return decl_typedef(pos, name, typespec);
}

AggregateField parse_decl_aggregate_field(void) {
    char *name = parse_name();
    expect_token(':');
    Typespec *typespec = parse_typespec();
    return (AggregateField){
        .name = name,
        .typespec = typespec,
    };
}

Decl *parse_decl_aggregate(DeclKind kind) {
	SourcePos pos = token.pos;
    char *name = parse_name();
	BUF(AggregateField *fields) = NULL; // @LEAK
	if (match_token('{')) {
		do {
			if (match_token('}')) break;
			da_push(fields, parse_decl_aggregate_field());
		} while (match_token(';'));
	} else if (match_token(';')) {
		// this is a forward declared aggregate
	} else {
		syntax_error("expected '{' or ';' to follow aggregate name, got '%s'", 
			token_kind_to_str(token.kind));
	}

	Decl *decl = decl_aggregate(pos, kind, name, fields, da_len(fields));
    return decl;
}

FuncParam parse_decl_func_param(void) {
	char *name = parse_name();
	expect_token(':');
	Typespec *typespec = parse_typespec();
	return (FuncParam){
		.name = name,
		.typespec = typespec,
	};
}

Decl *parse_decl_func(void) {
	SourcePos pos = token.pos;
    char *name = parse_name();
	bool is_variadic = false;
    expect_token('(');
    BUF(FuncParam *params) = NULL; // @LEAK
    if (!is_token(')')) {
		// parse first parameter
		if (match_token(TOKEN_ELLIPSIS)) {
			syntax_error("at least one parameter must precede '...' in a variadic function");
			is_variadic = true;
		} else { 
			da_push(params, parse_decl_func_param());
		}
		// parse remaining parameters
		while (match_token(',')) {
			if (match_token(TOKEN_ELLIPSIS)) {
				is_variadic = true;
			} else {
				if (is_variadic) syntax_error("no parameters can follow '...' in a variadic function");
				da_push(params, parse_decl_func_param());
			}
		}
    }
    expect_token(')');

    Typespec *ret_type = NULL;
    if (match_token(':')) {
        ret_type = parse_typespec();
    }

	StmtBlock block = {0};
	bool is_incomplete = false;
	if (match_token(';')) {
		is_incomplete = true;
	} else {
		block = parse_stmt_block();
	}

	return decl_func(pos, name, params, da_len(params), is_variadic, is_incomplete, ret_type, block);
}

Note parse_note(void) {
	char *name = parse_name();
	return (Note){ .name = name };
}

NoteList parse_note_list(void) {
	BUF(Note *notes) = NULL;
	SourcePos pos = token.pos;
	while (match_token('@')) {
		da_push(notes, parse_note());
	}
	return note_list(pos, notes, da_len(notes));
}

Decl *parse_decl_directive(void) {
	SourcePos pos = token.pos;
	char *name = parse_name();
	expect_token('(');
	BUF(DirectiveArg *args) = NULL;
	do {
		char *arg_name = parse_name();
		expect_token('=');
		Expr *expr = parse_expr();
		da_push(args, (DirectiveArg){arg_name, expr});
	} while (match_token(','));
	expect_token(')');
	Decl *decl = decl_directive(pos, name, args, da_len(args));
	da_free(args);
	return decl;
}

Decl *parse_decl_import(void) {
	SourcePos pos = token.pos;
	BUF(char *name_buf) = NULL;
	BUF(ImportItem *items) = NULL;
	bool is_relative = false;
	bool import_all = false;

	if (match_token('.')) {
		is_relative = true;
		da_printf(name_buf, ".%s", parse_name());
	} else {
		da_printf(name_buf, "%s", parse_name());
	}
	while (match_token('.')) {
		da_printf(name_buf, ".%s", parse_name());
	}
	char *name = str_intern(name_buf);

	// replace dots with slashes to get path
	char *tmp = is_relative ? name + 1 : name; // skip starting dot
	char *path = str_replace_char(tmp, '.', '/');
	char *package_path = str_intern(path);

	if (match_token('{')) {
		do {
			if (match_token(TOKEN_ELLIPSIS)) {
				import_all = true;
			} else {
				ImportItem item = {0};
				char *name = parse_name();
				if (match_token('=')) {
					item.rename = name;
					item.name = parse_name();
				} else {
					item.name = name;
				}
				da_push(items, item);
			}
		} while (match_token(','));
		expect_token('}');
	}

	return decl_import(pos, name, is_relative, import_all, package_path, items, da_len(items));
}

Decl *parse_decl(void) {
	Decl *decl = NULL;

	NoteList notes = {0};
	if (is_token('@')) {
		notes = parse_note_list();
	}

    if (match_keyword(keyword_enum)) {
        decl = parse_decl_enum();
    } else if (match_keyword(keyword_struct)) {
        decl = parse_decl_aggregate(DECL_STRUCT);
    } else if (match_keyword(keyword_union)) {
        decl = parse_decl_aggregate(DECL_UNION);
    } else if (match_keyword(keyword_var)) {
        decl = parse_decl_var();
    } else if (match_keyword(keyword_const)) {
        decl = parse_decl_const();
    } else if (match_keyword(keyword_func)) {
        decl = parse_decl_func();
    } else if (match_keyword(keyword_typedef)) {
        decl = parse_decl_typedef();
    } else if (match_keyword(keyword_import)) {
        decl = parse_decl_import();
    } else if (match_token('#')) {
        decl = parse_decl_directive();
    } else {
		syntax_error("Expected top level declaration, got %s", token_kind_to_str(token.kind));
	}

	decl->notes = notes;
    return decl;
}

void parse_expr_test(void) {
    init_keywords();
    char *exprs[] = {
        // base exprs
        "sizeof(int)",
        "sizeof(x)",
        "\"hey dude\"",
        "69",
        "3.14159",
        "fart",
        "cast(int, x)",
        "cast(float*, x)",
        "cast(Thing[32], x)",
        "(expr)",
        "Vec3{3,6,9}",
        // call exprs
        "image.width / image.height",
        "engage()",
        "draw_rect(0, 0, 25, 25, blue)",
        "nums[i]",
        // unary
        "-x + ~y + -j",
        "&window",
        "*P + *q",
        // mul ops
        "(x*x + y*y + z*z) / t",
        "(count % 4) + 1",
        "red << (3*8) | green << (2*8) | blue << (1*8) | alpha",
        "(gr >> 24) & 1",
        "num & mask",
        // add ops
        "q + b + (2 - j) - 11",
        "num ^ 0xc0ffee",
        "flag1 | flag2 | flag3",
        // cmp
        "x == y",
        "1+1 != 2*13",
        "poop() <= fart()",
        "32/6 >= t",
        "i < j",
        "k > 9999",
        // and
        "j && k",
        "(y & 1) && ((x >> 16) & 1)",
        // or
        "a || b",
        "x - 99 || b - 32",
        // ternary
        "dis_true ? 69 : 420",
        "a ? b ? 0 : 1 : c ? 2 : 3",
        "a ? (b ? 0 : 1) : 2",
    };

    for (size_t i = 0; i<ARRAY_COUNT(exprs); ++i) {
        init_stream("", exprs[i]);
        Expr *expr = parse_expr();
        print_expr(expr);
        printf("\n");
    }
}

void parse_stmt_test(void) {
    init_keywords();
    char *stmts[] = {
        // assign
        "x += 1;",
        "x -= 2;",
        "x *= 3;",
        "x /= 4;",
        "x %= 5;",
        "x &= 6;",
        "x |= 7;",
        "x ^= 8;",
        "x >>= 9;",
        "x <<= 10;",
        "count := 100;",
        "sum := 0;",
        "up := Vec3{0,1,0};",
        // switch
        "switch (target) { default: everyone_dies(); }",
        "switch (op) { }",
        "switch (op) { case OP_ADD: add(); break;  case OP_SUB: sub(); break;  case OP_MUL: mul(); break;  default: printf(\"Unknown op\\n\"); exit(1); break; }",
        // while
        "while (true) { do_stuff(); if (check_done()) { break; } else { append(a, next()); } cleanup(); }",
        "while (running) { update(); draw(); }",
        // do
        "do { x++; } while (x < 1000);",
        "do { stop(); drop(); roll(); } while (on_fire == true);",
        // for
        "for (i := 1; i < count + 1; i++) { sum += i; }",
        "for (; !quit; count++) { update(); }",
        "for (;;) { simulate(); }",
        // if
        "if (type == A) { procA(); } else if (type == B) { return; } else if (type == C) { procC(); } else { proc_default(); }",
        "if (dis_true) { x += 1; } else { x += 5; }",
        "if (x+3 > 5) { y = 12; };",
        //stmt block
        "{ v := Vec3{ 10, 20, 3 }; p = vec3_add(p, v); }",
        // return, continue, break
        "return sum / count;",
        "continue;",
        "break;",
        // inc, dec
        "i++;",
        "k--;",
    };

    for (size_t i = 0; i<ARRAY_COUNT(stmts); ++i) {
        init_stream("", stmts[i]);
        Stmt *stmt = parse_stmt();
        print_stmt(stmt);
        printf("\n");
    }
}

void parse_decl_test(void) {
    init_keywords();
    char *declarations[] = {
		// var args
        "func variadic(n: int, ...) {}", 

        // enum
        "enum Ops { OP_ADD = 0, OP_SUB, OP_MUL, }",
        "enum Things { A = 69, B = OP_ADD, C, D = 2+2, E = 0x32 }",

        // typedef
        "typedef Handle = int;",
        "typedef Mesh = Vec3[1024];",
        // struct, union
        "struct Node { x: int; next: Node*; }",
        "union bag { a: int; b: float; c: bool; d: Node*; e: int[2][4]; f: Fart[32]}",
        // var, const
        "const a = 420;",
        "const b = 69;",
        "var speed: float = 35.7;",
        "var c = 69 + 420 * 666;",
        "var d = Vec3{0,1,0};",
        "var e = (:Vec3){1,2,3};",
        "var f = {3, 6, 9};",
        "var g: Vec3 = {3, 6, 9};",
        // func
        "func pancake(count: int) { bake(ITEM_PANCAKE, count); }",
        "func main(argc: int, argv: char**): int { printf(\"Well, hello friends\\n\"); return 0; }",
        "func doodle(x: int, y: int): bool {}",
    };

    for (size_t i = 0; i<ARRAY_COUNT(declarations); ++i) {
        init_stream("", declarations[i]);
        Decl *decl = parse_decl();
        print_decl(decl);
        printf("\n");
    }
}

void parse_test(void) {
    // parse_expr_test();
    // parse_stmt_test();
    parse_decl_test();
}

