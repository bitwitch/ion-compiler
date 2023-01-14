/*
 * Grammar for simple arithmetic expressions:
 *
 * expr_base   = integer | '(' expr ')'
 * expr_unary  = ([~-] expr_unary)* | expr_base
 * expr_mul    = expr_unary ([* % / >> << &] expr_unary)*
 * expr_add    = expr_mul ([+-^] expr_mul)*
 * expr        = expr_add
 *
 */

typedef enum {
    // reserving values 0 - 127 for tokens that are single ascii characters
	TOKEN_INT = 128,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT
} TokenKind;

typedef struct {
	TokenKind kind;
	int32_t val;
} Token;

typedef enum {
    EXPR_NONE,
    EXPR_UNARY,
    EXPR_BINARY,
    EXPR_INT
} ExprKind;

typedef struct Expr Expr;
struct Expr {
    ExprKind kind;
    TokenKind op;
    int32_t val;
    Expr *left, *right;
};

Token token = {0};
char *stream;


Expr *parse_expr(void);
Expr *expr_unary(TokenKind op, Expr *expr);
Expr *expr_binary(TokenKind op, Expr *left, Expr *right);
Expr *expr_int(int32_t val);

// Warning! this returns a pointer to a static buffer, so its contents will
// change on each call
char *str_token_kind(TokenKind kind) {
    static char str[64] = {0};
    if (kind == TOKEN_INT) {
        sprintf(str, "integer");
    } else {
        if (kind < 128 && isprint(kind))
            sprintf(str, "%c", kind);
        else if (kind == TOKEN_LSHIFT)
            sprintf(str, "<<");
        else if (kind == TOKEN_RSHIFT)
            sprintf(str, ">>");
        else 
            sprintf(str, "<ASCII %d>", kind);
    }
    return str;
}

void next_token(void) {
	while (isspace(*stream)) ++stream;

	if (isdigit(*stream)) {
		token.kind = TOKEN_INT;
		token.val = 0;
		while (isdigit(*stream)) {
			token.val *= 10;
			token.val += *stream - '0';
			++stream;
		}
	} else {
        TokenKind op = *stream++;
        if (op == '<' && *stream == '<') {
            op = TOKEN_LSHIFT;
            stream++;
        } else if (op == '>' && *stream == '>') {
            op = TOKEN_RSHIFT;
            stream++;
        }
		token.kind = op;
	}
}

bool is_token(TokenKind kind) {
    return token.kind == kind;
}

bool match_token(TokenKind kind) {
    if (token.kind == kind) {
        next_token();
        return true;
    }
    return false;
}

void expect_token(TokenKind kind) {
    if (token.kind == kind) {
        next_token();
    } else {
        fprintf(stderr, "Syntax Error: Expected token '%s', got '%s'\n", 
                str_token_kind(kind), str_token_kind(token.kind));
        exit(1);
    }
}


Expr *expr_alloc(ExprKind kind) {
    Expr *expr = xcalloc(1, sizeof(Expr));
    expr->kind = kind;
    return expr;
}

Expr *expr_int(int32_t val) {
    Expr *expr = expr_alloc(EXPR_INT);
    expr->val  = val;
    return expr;
}

Expr *expr_unary(TokenKind op, Expr *operand) {
    Expr *expr  = expr_alloc(EXPR_UNARY);
    expr->op    = op;
    expr->left  = NULL;
    expr->right = operand;
    return expr;
}

Expr *expr_binary(TokenKind op, Expr *left, Expr *right) {
    Expr *expr  = expr_alloc(EXPR_BINARY);
    expr->op    = op;
    expr->left  = left;
    expr->right = right;
    return expr;
}

Expr *parse_expr_base(void) {
    Expr *expr = NULL;
    if (is_token(TOKEN_INT)) {
        expr = expr_int(token.val);
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

uint8_t *append_lit(uint8_t *code, int32_t val) {
    da_push(code, OP_LIT);
    for (int num_bytes = sizeof(val); num_bytes > 0; --num_bytes) {
        da_push(code, val & 0xFF);
        val >>= 8;
    }
    return code;
}

uint8_t ascii_to_opcode[256] = {
    ['~'] = OP_NOT,
    // ['-'] = OP_NEG, // this is the same as OP_SUB, need to discriminate the two by checking if unary

    ['*'] = OP_MUL,
    ['/'] = OP_DIV,
    ['%'] = OP_MOD,
    ['&'] = OP_AND,
    ['+'] = OP_ADD,
    ['-'] = OP_SUB,
    ['|'] = OP_OR,
    ['^'] = OP_XOR,

    [TOKEN_LSHIFT] = OP_SHL,
    [TOKEN_RSHIFT] = OP_SHR
};

uint8_t *append_op(uint8_t *code, Expr *expr) {
    uint8_t op = ascii_to_opcode[expr->op];
    if (expr->op == '-' && expr->kind == EXPR_UNARY)
        op = OP_NEG;
    da_push(code, op);
    return code;
}


uint8_t *generate_bytecode_recur(uint8_t *code, Expr *expr) {
    if (expr == NULL) return code;

    ExprKind kind = expr->kind;
    assert(kind != 0);

    if (kind == EXPR_INT)
        code = append_lit(code, expr->val);
    else {
        code = generate_bytecode_recur(code, expr->left);
        code = generate_bytecode_recur(code, expr->right);
        code = append_op(code, expr);
    }

    return code;
}

uint8_t *generate_bytecode(Expr *expr) {
    uint8_t *code = NULL;
    code = generate_bytecode_recur(code, expr);
    da_push(code, OP_HALT);
    return code;
}

void print_expression(Expr *expr) {
    if (expr == NULL) return;

    assert(expr->kind != 0);

    if (expr->kind == EXPR_INT) 
        printf("%d", expr->val);
    else 
        printf("(%s ", str_token_kind(expr->op));

    if (expr->left) {
        print_expression(expr->left);
        printf(" ");
    }
    if (expr->right) {
        print_expression(expr->right);
        printf(")");
    }
}

/*
#define TEST_PARSE_EXPRESSION(e) assert(parse_expression(#e) == (e))
void parse_test(void) {
    TEST_PARSE_EXPRESSION(3);
    TEST_PARSE_EXPRESSION(-3);
    TEST_PARSE_EXPRESSION(7+-4);
    TEST_PARSE_EXPRESSION((3));
    TEST_PARSE_EXPRESSION(60 + 9);
    TEST_PARSE_EXPRESSION(60 + 9 + 351);
    TEST_PARSE_EXPRESSION(300*2 + 33+33);
    TEST_PARSE_EXPRESSION(300*2 + 33+33);
    TEST_PARSE_EXPRESSION(300 * (2+33) + 33);
}
#undef TEST_PARSE_EXPRESSION
*/




