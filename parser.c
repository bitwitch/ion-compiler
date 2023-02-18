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
    expect_token('{');
    Expr **exprs = NULL;
    do {
        da_push(exprs, parse_expr());
    } while (match_token(','));
    expect_token('}');
    return expr_compound(type, exprs, da_len(exprs));
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
        if (is_token('{')) {
            Typespec *type = typespec_name(name);
            expr = parse_expr_compound(type);
        } else {
            expr = expr_name(name);
        }
    } else if (match_keyword(keyword_cast)) {
        expect_token('(');
        Typespec *type = parse_type();
        expect_token(',');
        Expr *sub_expr = parse_expr();
        expect_token(')');
        expr = expr_cast(type, sub_expr);
    } else if (match_token('(')) {
        // compound literal
        if (match_token(':')) {
            Typespec *type = parse_type();
            expect_token(')');
            expr = parse_expr_compound(type);
        } else {
            expr = parse_expr();
            expect_token(')');
        }

    } else if (is_token('{')) {
        expr = parse_expr_compound(NULL);
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

Expr *parse_expr_cmp(void) {
    Expr *expr = parse_expr_add();
    while (is_token_cmp()) {
        TokenKind op = token.kind;
        next_token();
        expr = expr_cmp(op, expr, parse_expr_add());
    }
    return expr;
}

Expr *parse_expr_and(void) {
    Expr *expr = parse_expr_cmp();
    while (match_token(TOKEN_LOGICAL_AND)) {
        expr = expr_and(expr, parse_expr_cmp());
    }
    return expr;
}

Expr *parse_expr_or(void) {
    Expr *expr = parse_expr_and();
    while (match_token(TOKEN_LOGICAL_OR)) {
        expr = expr_or(expr, parse_expr_and());
    }
    return expr;
}

Expr *parse_expr_ternary(void) {
    Expr *expr = parse_expr_or();
    if (match_token('?')) {
        Expr *then_expr = parse_expr_ternary();
        expect_token(':');
        Expr *else_expr = parse_expr_ternary();
        expr = expr_ternary(expr, then_expr, else_expr);
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
    Stmt **stmts = NULL;
    expect_token('{');
    while (!is_token('}'))
        da_push(stmts, parse_stmt());
    expect_token('}');
    return stmt_block(stmts, da_len(stmts));
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

    return stmt;
}


Stmt *parse_stmt_for(void) {
    expect_token('(');
    Stmt *init = parse_simple_stmt();
    expect_token(';');
    Expr *cond = parse_expr();
    expect_token(';');
    Stmt *next = parse_simple_stmt();
    expect_token(')');
    StmtBlock block = parse_stmt_block();
    return stmt_for(init, cond, next, block);
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
        Stmt *stmt = parse_simple_stmt();
        expect_token(';');
        return stmt;
    }
}



EnumItem *parse_enum_items(void) {
    assert(0);
    EnumItem *items = NULL;
    return items;
}

AggregateField *parse_decl_aggregate_fields(void) {
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

void parse_expr_test(void) {
    init_keywords();
    char *exprs[] = {
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

    for (size_t i = 0; i<array_count(exprs); ++i) {
        init_stream(exprs[i]);
        Expr *expr = parse_expr();
        print_expr(expr);
        printf("\n");
    }
}

void parse_stmt_test(void) {
    init_keywords();
    char *stmts[] = {
        "count := 100;",
        "sum := 0;",
        "for (i := 1; i < count + 1; i++) { sum += i; }",
        "return sum / count;",

        "up := Vec3{0,1,0};",
        "i++;",
        "k--;",
    };

    for (size_t i = 0; i<array_count(stmts); ++i) {
        init_stream(stmts[i]);
        Stmt *stmt = parse_stmt();
        print_stmt(stmt);
        printf("\n");
    }
}

void parse_decl_test(void) {
    init_keywords();
    char *declarations[] = {
        "func pancake(count: int) { bake(ITEM_PANCAKE, count); }",
        "func main(argc: int, argv: char**): int { printf(\"Well, hello friends\\n\"); return 0; }",
        "func doodle(x: int, y: int): bool {}",
        "struct Node { x: int; next: Node*; }",
        "union bag { a: int; b: float; c: bool; d: Node*; e: int[2][4]; f: Fart[32]}",
        "const a = 420;",
        "const b = 69;",
        "var speed: float = 35.7;",
        "var c = 69 + 420 * 666;",
        "var d = Vec3{0,1,0};",
        "var e = (:Vec3){1,2,3};",
        "var f = {3, 6, 9};",
        "var g: Vec3 = {3, 6, 9};",
    };

    for (size_t i = 0; i<array_count(declarations); ++i) {
        init_stream(declarations[i]);
        Decl *decl = parse_decl();
        print_decl(decl);
        printf("\n");
    }
}

void parse_test(void) {
    /*parse_expr_test();*/
    parse_stmt_test();
    /*parse_decl_test();*/
}

