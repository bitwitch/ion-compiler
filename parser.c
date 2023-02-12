Expr *parse_expr(void);
Stmt *parse_stmt(void);


char *parse_name(void) {
    if (is_token(TOKEN_NAME)) {
        char *name = token.name;
        next_token();
        return name;
    } else {
        syntax_error("Expected name, got %s", str_token_kind(token.kind));
        return NULL;
    }
}

Typespec *parse_type_base(void) {
    if (is_token(TOKEN_NAME)) {
        char *name = parse_name();
        return typespec_name(name);
    } else {
        syntax_error("Unexpected token in type: %s", token_info());
        return NULL;
    }
}

// int[2][4]
Typespec *parse_type(void) {
    Typespec *type = parse_type_base();
    while (is_token('[') || is_token('*')) {
        if (match_token('[')) {
            Expr *size = parse_expr();
            expect_token(']');
            type = typespec_array(type, size);
        } else {
            assert(is_token('*'));
            next_token();
            type = typespec_ptr(type);
        }
    }

    return type;
}


Expr *parse_expr_compound(Typespec *type) {
    assert(0);
    return NULL;
}

Expr *parse_expr_base(void) {
    Expr *expr = NULL;
    if (is_token(TOKEN_INT)) {
        expr = expr_int(token.int_val);
        next_token();
    } else if (is_token(TOKEN_FLOAT)) {
        expr = expr_float(token.float_val);
        next_token();
    } else if (is_token(TOKEN_STR)) {
        expr = expr_str(token.str_val);
        next_token();
    } else if (is_token(TOKEN_NAME)) {
        char *name = token.name;
        next_token();
        if (match_token('{'))
            expr = parse_expr_compound(NULL);
        else
            expr = expr_name(name);
    } else if (match_keyword(keyword_cast)) {
        expect_token('(');
        Typespec *type = parse_type();
        expect_token(',');
        Expr *sub_expr = parse_expr();
        expect_token(')');
        expr = expr_cast(type, sub_expr);
    } else if (match_token('(')) {
        if (match_token(':')) {
            Typespec *type = parse_type();
            expect_token(')');
            expr = parse_expr_compound(type);
        } else {
            expr = parse_expr();
            expect_token(')');
        }
    } else {
        syntax_error("Unexpected token in base expression: '%s'", str_token_kind(token.kind));
    }

    return expr;
}

Expr *parse_expr_call(void) {
    Expr *expr = parse_expr_base();

    while (is_token('(') || is_token('[') || is_token('.')) {
        if (match_token('(')) {
            Expr **args = NULL;
            if (!is_token(')')) {
                do {
                    da_push(args, parse_expr());
                } while (match_token(','));
            }
            expect_token(')');
            expr = expr_call(expr, args, da_len(args));
        } else if (match_token('[')) {
            Expr *index = parse_expr();
            expect_token(']');
            expr = expr_index(expr, index);
        } else {
            assert(is_token('.'));
            next_token();
            char *field = token.name;
            expect_token(TOKEN_NAME);
            expr = expr_field(expr, field);
        }
    }
    return expr;
}


Expr *parse_expr_unary(void) {
    if (is_token('-') || is_token('~')) {
        TokenKind op = token.kind;
        next_token();
        return expr_unary(op, parse_expr_unary());
    }
    return parse_expr_call();
}

Expr *parse_expr_mul(void) {
    Expr *expr = parse_expr_unary();
    
    while (is_token('*') || is_token('/') || is_token('%') || 
           is_token('&') || is_token(TOKEN_LSHIFT) || is_token(TOKEN_RSHIFT))
    {
        TokenKind op = token.kind;
        next_token();
        expr = expr_binary(op, expr, parse_expr_unary());
    }

    return expr;
}

Expr *parse_expr_add(void) {
    Expr *expr = parse_expr_mul();

    while (is_token('+') || is_token('-') || is_token('^')) {
        TokenKind op = token.kind;
        next_token();
        expr = expr_binary(op, expr, parse_expr_mul());
    }

    return expr;
}

Expr *parse_expr(void) {
    return parse_expr_add();
}

Expr *parse_expression(char *source) {
    stream = source;
    next_token();
    return parse_expr();
}


Stmt *parse_stmt_return(void) {
    Expr *expr = NULL;
    if (!is_token(';'))
        expr = parse_expr();
    expect_token(';');
    return stmt_return(expr);
}

Stmt *parse_stmt_if(void) {
    assert(0);
    return NULL;
}

Stmt *parse_stmt_for(void) {
    assert(0);
    return NULL;
}

Stmt *parse_stmt_do(void) {
    assert(0);
    return NULL;
}

Stmt *parse_stmt_while(void) {
    assert(0);
    return NULL;
}

Stmt *parse_stmt_switch(void) {
    assert(0);
    return NULL;
}

bool is_assign_op(void) {
    return token.kind == TOKEN_EQ        ||
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
    Expr *expr = parse_expr();
    Stmt *stmt = NULL;
    if (match_token(TOKEN_AUTO_ASSIGN)) {
        if (expr->kind != EXPR_NAME) {
            syntax_error(":= must be preceded by a name");
            return NULL;
        }
        stmt = stmt_init(expr->name, parse_expr());
    } else if (is_assign_op()) {
        TokenKind op = token.kind;
        next_token();
        stmt = stmt_assign(op, expr, parse_expr());
    } else if (match_token(TOKEN_INC)) {
        stmt = stmt_assign(TOKEN_INC, expr, NULL);
    } else if (match_token(TOKEN_DEC)) {
        stmt = stmt_assign(TOKEN_DEC, expr, NULL);
    } else {
        stmt = stmt_expr(expr);
    }

    expect_token(';');

    return stmt;
}


StmtBlock parse_stmt_block(void) {
    Stmt **stmts = NULL;
    expect_token('{');
    while (!is_token('}'))
        da_push(stmts, parse_stmt());
    expect_token('}');
    return stmt_block(stmts, da_len(stmts));
}


Stmt *parse_stmt(void) {
    if (match_keyword(keyword_return))
        return parse_stmt_return();
    else if (match_keyword(keyword_continue))
        return stmt_alloc(STMT_CONTINUE);
    else if (match_keyword(keyword_break))
        return stmt_alloc(STMT_BREAK);
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
    else if (match_token('{'))
        return stmt_brace_block(parse_stmt_block());
    else {
        return parse_simple_stmt();
    }
}


EnumItem *parse_enum_items(void) {
    assert(0);
    EnumItem *items = NULL;
    return items;
}

AggregateField *parse_decl_aggregate_fields(void) {

    AggregateField *fields = NULL;
    return fields;
}

Decl *parse_decl_enum(void) {
    char *name = parse_name();
    expect_token('{');
    Decl *decl = decl_enum(name, parse_enum_items());
    expect_token('}');
    return decl;
}

Decl *parse_decl_const(void) {
    char *name = parse_name();
    expect_token('=');
    Expr *expr = parse_expr();
    return decl_const(name, expr);
}

Decl *parse_decl_var(void) {
    char *name = parse_name();
    Typespec *type = NULL;
    Expr *expr = NULL;
    if (match_token(':')) {
        type = parse_type();
    }
    if (match_token('=')) {
        expr = parse_expr();
    }
    return decl_var(name, type, expr);
}

AggregateField parse_decl_aggregate_field(void) {
    char *name = parse_name();
    expect_token(':');
    Typespec *type = parse_type();
    return (AggregateField){
        .name = name,
        .type = type,
    };
}


// struct Node { i: int; next: Node*; }

Decl *parse_decl_aggregate(DeclKind kind) {
    char *name = parse_name();
    expect_token('{');
    AggregateField *fields = NULL;
    do {
        if (match_token('}')) break;
        da_push(fields, parse_decl_aggregate_field());
    } while (match_token(';'));

    Decl *decl = decl_aggregate(kind, name, fields, da_len(fields));
    return decl;
}

FuncParam parse_decl_func_param(void) {
    char *name = parse_name();
    expect_token(':');
    Typespec *type = parse_type();
    return (FuncParam){
        .name = name,
        .type = type,
    };
}

Decl *parse_decl_func(void) {
    char *name = parse_name();
    expect_token('(');
    FuncParam *params = NULL;
    if (!is_token(')')) {
        do {
            da_push(params, parse_decl_func_param());
        } while (match_token(','));
    }
    expect_token(')');

    Typespec *ret_type = NULL;
    if (match_token(':')) {
        ret_type = parse_type();
    }

    StmtBlock block = parse_stmt_block();

    return decl_func(name, params, da_len(params), ret_type, block);
}


Decl *parse_decl(void) {
    if (match_keyword(keyword_enum)) {
        return parse_decl_enum();
    } else if (match_keyword(keyword_struct)) {
        return parse_decl_aggregate(DECL_STRUCT);
    } else if (match_keyword(keyword_union)) {
        return parse_decl_aggregate(DECL_UNION);
    } else if (match_keyword(keyword_var)) {
        return parse_decl_var();
    } else if (match_keyword(keyword_const)) {
        return parse_decl_const();
    } else if (match_keyword(keyword_func)) {
        return parse_decl_func();
    }
    syntax_error("Expected top level declaration, got %s", str_token_kind(token.kind));
    return NULL;
}

void parse_test(void) {
    init_keywords();
    char *declarations[] = {
        "func pancake(count: int) { bake(ITEM_PANCAKE, count); }",
        "func main(argc: int, argv: char**): int { printf(\"Well, hello friends\\n\"); return 0; }",
        "struct Node { x: int; next: Node*; }",
        "union bag { a: int; b: float; c: bool; d: Node*; e: int[2][4]; f: Fart[32]}",
        "func doodle(x: int, y: int): bool {}",
        "var speed: float = 35.7;",
        "const a = 420;",
        "const b = 69;",
        "var c = 69 + 420 * 666;",
        /*"var up: Vec3 = {0,1,0};",*/
    };

    for (size_t i = 0; i<array_count(declarations); ++i) {
        init_stream(declarations[i]);
        Decl *decl = parse_decl();
        print_decl(decl);
        printf("\n");
    }
}

