Expr *parse_expr(void);

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

Expr *parse_expr_base(void) {
    Expr *expr = NULL;
    if (is_token(TOKEN_INT)) {
        expr = expr_int(token.int_val);
        next_token();
    } else if (is_token(TOKEN_FLOAT)) {
        expr = expr_float(token.float_val);
        next_token();
    } else if (match_token('(')) {
        expr = parse_expr();
        expect_token(')');
    } else {
        syntax_error("Expected integer or '(', got %s", str_token_kind(token.kind));
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


StmtBlock parse_stmt_block(void) {
    assert(0);
    StmtBlock block = {0};
    return block;
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
        da_push(params, parse_decl_func_param());
        while (match_token(',')) {
            da_push(params, parse_decl_func_param());
        }
    }
    expect_token(')');

    Typespec *ret_type = NULL;
    if (match_token(':')) {
        ret_type = parse_type();
    }

    // TODO(shaw): change this back after implementing parse_stmt_block
    /*StmtBlock block = parse_stmt_block();*/
    StmtBlock block = {0};

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


void print_expr(Expr *expr) {
    if (expr == NULL) {
        printf("NULL");
        return;
    }

    switch (expr->kind) {
    case EXPR_INT: 
        printf("%d", expr->int_val);    
        break;
    case EXPR_FLOAT: 
        printf("%f", expr->float_val); 
        break;
    case EXPR_UNARY: 
        printf("(%c ", expr->unary.op); 
        print_expr(expr->unary.expr);
        printf(")");
        break;
    case EXPR_BINARY: 
        printf("(%c ", expr->binary.op); 
        print_expr(expr->binary.left);
        printf(" ");
        print_expr(expr->binary.right);
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

void print_type(Typespec *type) {
    if (type == NULL) {
        printf("NULL");
        return;
    }

    switch (type->kind) {
    case TYPESPEC_NAME:
        printf("%s", type->name);
        break;
    case TYPESPEC_POINTER:
        printf("(ptr ");
        print_type(type->ptr.elem);
        printf(")");
        break;
    case TYPESPEC_ARRAY:
        printf("(array ");
        print_type(type->array.elem);
        printf(" ");
        print_expr(type->array.size);
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

// struct Node { i: int; next: Node* }
// (struct Node 
//   (i int) 
//   (next (ptr Node)))
// 

#define INDENT_WIDTH 4
void print_decl(Decl *decl) {
    static int indent = 0;
    printf("%*s", indent*INDENT_WIDTH, " ");

    switch (decl->kind) {
    case DECL_CONST:
        printf("(const %s ", decl->name);
        print_expr(decl->const_decl.expr);
        printf(")");
        break;
    case DECL_VAR:
        printf("(var %s ", decl->name);
        print_type(decl->var.type);
        printf(" ");
        print_expr(decl->var.expr);
        printf(")");
        break;
    case DECL_FUNC:
        printf("(func %s (", decl->name);
        for (int i=0; i<decl->func.num_params; ++i) {
            FuncParam p = decl->func.params[i];
            printf(" %s ", p.name);
            print_type(p.type);
        }
        printf(" )");
        if (decl->func.ret_type) {
            printf(" ");
            print_type(decl->func.ret_type);
        }
        printf(")");

        // TODO(shaw): print statement block

        break;

    case DECL_STRUCT:
        printf("(struct %s ", decl->name);
        ++indent;
        for (int i=0; i<decl->aggregate.num_fields; ++i) {
            printf("\n%*s", indent*INDENT_WIDTH, " ");
            AggregateField f = decl->aggregate.fields[i];
            printf("(%s ", f.name);
            print_type(f.type);
            printf(")");
        }
        --indent;
        printf(")");
        break;

    default:
        assert(0 && "Unknown decl kind");
        break;
    }
}
#undef INDENT_WIDTH

void parse_test(void) {
    init_keywords();

    char *declarations[] = {
        "struct Node { x: int; next: Node*; }",
        "func doodle(x: int, y: int): bool;",
        "var speed: float = 35.7;",
        "const a = 420;",
        "const b = 69;",
        "var c = 69 + 420 * 666;",
        /*"var up: Vec3 = {0,1,0};",*/
        /*"func pancake(i: int) { bake(); }",*/
    };

    for (size_t i = 0; i<array_count(declarations); ++i) {
        init_stream(declarations[i]);
        Decl *decl = parse_decl();
        print_decl(decl);
        printf("\n");
    }
}

