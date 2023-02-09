// Expressions
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

Expr *expr_float(double val) {
    Expr *expr = expr_alloc(EXPR_FLOAT);
    expr->float_val = val;
    return expr;
}

Expr *expr_unary(TokenKind op, Expr *operand) {
    Expr *expr = expr_alloc(EXPR_UNARY);
    expr->unary.op = op;
    expr->unary.expr = operand;
    return expr;
}

Expr *expr_binary(TokenKind op, Expr *left, Expr *right) {
    Expr *expr  = expr_alloc(EXPR_BINARY);
    expr->binary.op    = op;
    expr->binary.left  = left;
    expr->binary.right = right;
    return expr;
}

void expr_test(void) {
    Expr *expr = expr_int(69);
    assert(expr->kind == EXPR_INT);
    assert(expr->int_val == 69);
}

// Types
Typespec *typespec_alloc(TypespecKind kind) {
    Typespec *typespec = xcalloc(1, sizeof(Typespec));
    typespec->kind = kind;
    return typespec;
}

Typespec *typespec_name(char *name) {
    Typespec *typespec = typespec_alloc(TYPESPEC_NAME);
    typespec->name = name;
    return typespec;
}

Typespec *typespec_ptr(Typespec *elem) {
    Typespec *typespec = typespec_alloc(TYPESPEC_POINTER);
    typespec->ptr.elem = elem;
    return typespec;
}

Typespec *typespec_array(Typespec *elem, Expr *size) {
    Typespec *typespec = typespec_alloc(TYPESPEC_ARRAY);
    typespec->array.elem = elem;
    typespec->array.size = size;
    return typespec;
}

Typespec *typespec_func(Typespec **args, int num_args, Typespec *ret) {
    Typespec *typespec = typespec_alloc(TYPESPEC_FUNC);
    typespec->func.args = args;
    typespec->func.num_args = num_args;
    typespec->func.ret = ret;
    return typespec;
}

// Declarations
Decl *decl_alloc(DeclKind kind) {
    Decl *decl = xcalloc(1, sizeof(Decl));
    decl->kind = kind;
    return decl;
}

Decl *decl_const(char *name, Expr *expr) {
    Decl *decl = decl_alloc(DECL_CONST);
    decl->name = name;
    decl->const_decl.expr = expr;
    return decl;
}

Decl *decl_var(char *name, Typespec *type, Expr *expr) {
    Decl *decl = decl_alloc(DECL_VAR);
    decl->name = name;
    decl->var.type = type;
    decl->var.expr = expr;
    return decl;
}

Decl *decl_enum(char *name, EnumItem *items) {
    assert(0);
    Decl *decl = NULL;
    return decl;
}

Decl *decl_aggregate(DeclKind kind, char *name, AggregateField *fields, int num_fields) {
    assert(kind == DECL_STRUCT || kind == DECL_UNION);
    Decl *decl = decl_alloc(kind);
    decl->name = name;
    decl->aggregate.fields = fields;
    decl->aggregate.num_fields = num_fields;
    return decl;
}

Decl *decl_func(char *name, FuncParam *params, int num_params, Typespec *ret_type, StmtBlock block) {
    Decl *decl = decl_alloc(DECL_FUNC);
    decl->name = name;
    decl->func.params = params;
    decl->func.num_params = num_params;
    decl->func.ret_type = ret_type;
    decl->func.block = block;
    return decl;
}


