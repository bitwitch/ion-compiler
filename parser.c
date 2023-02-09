Expr *parse_expr(void);

char *parse_name(void) {
    assert(0);
    char *name = NULL;
    return name;
}

Expr *parse_expr_base(void) {
    Expr *expr = NULL;
    if (is_token(TOKEN_INT)) {
        expr = expr_int(token.int_val);
        next_token();
    } else if (match_token('(')) {
        expr = parse_expr();
        expect_token(')');
    } else {
        fprintf(stderr, "Syntax Error: Expected integer or '(', got %s\n", str_token_kind(token.kind));
        exit(1);
    }

    return expr;
}

Expr *parse_expr_unary(void) {
    if (is_token('-') || is_token('~')) {
        TokenKind op = token.kind;
        next_token();
        return expr_unary(op, parse_expr_unary());
    }
    return parse_expr_base();
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

EnumItem *parse_enum_items(void) {
    assert(0);
    EnumItem *items = NULL;
    return items;
}

AggregateField *parse_fields(void) {
    assert(0);
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
    Expr *expr = NULL;
    if (is_token('=')) {
        next_token();
        expr = parse_expr();
    }
    return decl_var(name, expr);
}

Decl *parse_decl_aggregate(AggregateKind kind) {
    char *name = parse_name();
    expect_token('{');
    Decl *decl = decl_aggregate(kind, name, parse_fields());
    expect_token('}');
    return decl;
}

Decl *parse_decl_func(void) {
    assert(0);
    Decl *decl = NULL;
    return decl;
}


Decl *parse_decl(void) {
    if (match_keyword(keyword_enum)) {
        return parse_decl_enum();
    } else if (match_keyword(keyword_struct)) {
        return parse_decl_aggregate(AGGREGATE_STRUCT);
    } else if (match_keyword(keyword_union)) {
        return parse_decl_aggregate(AGGREGATE_UNION);
    } else if (match_keyword(keyword_var)) {
        return parse_decl_var();
    } else if (match_keyword(keyword_const)) {
        return parse_decl_const();
    } else if (match_keyword(keyword_func)) {
        return parse_decl_func();
    }
    syntax_error("Expected top level declaration, got %s\n", str_token_kind(token.kind));
    return NULL;
}

void print_decl(Decl *decl) {
    switch (decl->kind) {
        case DECL_CONST:
            printf("(const %s ", decl->name);

            break;
        case DECL_FUNC:
            break;
        default:
            assert(0 && "Unknown decl kind");
            break;
    }
}


void parse_test(void) {
    init_keywords();

    char *declarations[] = {
        "const a = 69;",
        "const b = 420;"
    };

    for (size_t i = 0; i<array_count(declarations); ++i) {
        init_stream(declarations[i]);
        Decl *decl = parse_decl();
        print_decl(decl);
    }
}

