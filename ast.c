/*
Declarations:

decl = 'enum' enum_decl
     | 'struct' aggregate_decl
     | 'union' aggregate_decl
     | 'var' var_decl
     | 'const' const_decl
     | 'typedef' typedef_decl
     | 'func' func_decl

Statements:

stmt_block = '{' stmt* '}'
stmt = 'return' expr? ';'
     | 'continue' ';'
     | 'break' ';'
     | stmt_block
     | 'if' '(' expr ')' stmt_block elseif* ('else' stmt_block)?
     | 'for' '(' for_init? ';' expr? ';' for_next? ')' stmt_block
     | 'do' stmt_block 'while' '(' expr ')' ';'
     | 'while' '(' expr ')' stmt_block
     | 'switch' '(' expr ')' case* ('default' ':' stmt*)?
     | expr (INC | DEC | assign_op expr)?



Expressions:

typespec = NAME | '(' ':' type ')'
base_expr = INT
          | FLOAT
          | STR
          | NAME
          | typespec? '{' expr_list '}'
          | CAST '(' expr ')'
          | '(' expr ')'
compound_expr =  base_expr ('(' param* ')' | '[' expr ']' | '.' NAME)*
unary_expr = [+-&*~] unary_expr
           | compound_expr
mul_op = '*' | '/' | '%' | '&' | LSHIFT | RSHIFT
mul_expr = unary_expr (mul_op unary_expr)*
add_op = '+' | '-' | '|' | '^'
add_expr = mul_expr (add_op mul_expr)*
cmp_op = EQ | NOTEQ | LT | GT | LTEQ | GTEQ
cmp_expr = add_expr (cmp_op add_expr)*
and_expr = cmp_expr (AND cmp_expr)*
or_expr = and_expr (OR and_expr)*
ternary_expr = or_expr ('?' ternary_expr ':' ternary_expr)?
expr = ternary_expr
*/


// Expressions
Expr *expr_unary(TokenKind op, Expr *expr);
Expr *expr_binary(TokenKind op, Expr *left, Expr *right);
Expr *expr_int(int32_t val);


Expr *expr_alloc(ExprKind kind) {
    Expr *expr = xcalloc(1, sizeof(Expr));
    expr->kind = kind;
    return expr;
}

Expr *expr_int(int32_t val) {
    Expr *expr = expr_alloc(EXPR_INT);
    expr->int_val = val;
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


void expr_test(void) {
    Expr *expr = expr_int(69);
    assert(expr->kind == EXPR_INT);
    assert(expr->int_val == 69);
}


// Declarations
Decl *decl_const(char *name, Expr *expr) {
    assert(0);
    Decl *decl = NULL;
    return decl;
}

Decl *decl_var(char *name, Expr *expr) {
    assert(0);
    Decl *decl = NULL;
    return decl;
}

Decl *decl_enum(char *name, EnumItem *items) {
    assert(0);
    Decl *decl = NULL;
    return decl;
}

Decl *decl_aggregate(AggregateKind kind, char *name, AggregateField *fields) {
    assert(0);
    Decl *decl = NULL;
    return decl;
}





