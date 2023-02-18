Arena ast_arena;

void *ast_memdup(void *src, size_t size) {
    void *new_mem = arena_alloc(&ast_arena, size);
    memcpy(new_mem, src, size);
    return new_mem;
}


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

Expr *expr_str(char *val) {
    Expr *expr = expr_alloc(EXPR_STR);
    expr->str_val = val;
    return expr;
}

Expr *expr_name(char *name) {
    Expr *expr = expr_alloc(EXPR_NAME);
    expr->name = name;
    return expr;
}

Expr *expr_cast(Typespec *type, Expr *expr) {
    Expr *new_expr = expr_alloc(EXPR_CAST);
    new_expr->cast.type = type;
    new_expr->cast.expr = expr;
    return new_expr;
}

Expr *expr_unary(TokenKind op, Expr *operand) {
    Expr *expr = expr_alloc(EXPR_UNARY);
    expr->unary.op = op;
    expr->unary.expr = operand;
    return expr;
}

Expr *expr_binary(TokenKind op, Expr *left, Expr *right) {
    Expr *expr = expr_alloc(EXPR_BINARY);
    expr->binary.op    = op;
    expr->binary.left  = left;
    expr->binary.right = right;
    return expr;
}

Expr *expr_ternary(Expr *cond, Expr *then_expr, Expr *else_expr) {
    Expr *expr = expr_alloc(EXPR_TERNARY);
    expr->ternary.cond = cond;
    expr->ternary.else_expr = else_expr;
    expr->ternary.then_expr = then_expr;
    return expr;
}


Expr *expr_call(Expr *expr, Expr **args, int num_args) {
    Expr *new_expr = expr_alloc(EXPR_CALL);
    new_expr->call.expr = expr;
    new_expr->call.args = args;
    new_expr->call.num_args = num_args;
    return new_expr;
}


Expr *expr_index(Expr *expr, Expr *index) {
    Expr *new_expr = expr_alloc(EXPR_INDEX);
    new_expr->index.expr = expr;
    new_expr->index.index = index;
    return new_expr;
}

Expr *expr_field(Expr *expr, char *field) {
    Expr *new_expr = expr_alloc(EXPR_FIELD);
    new_expr->field.expr = expr;
    new_expr->field.name = str_intern(field);
    return new_expr;
}

Expr *expr_compound(Typespec* type, Expr **args, int num_args) {
    Expr *expr = expr_alloc(EXPR_COMPOUND);
    expr->compound.type = type;
    expr->compound.args = args;
    expr->compound.num_args = num_args;
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

// Statements
Stmt *stmt_alloc(StmtKind kind) {
    Stmt *stmt = xcalloc(1, sizeof(Stmt));
    stmt->kind = kind;
    return stmt;
}

StmtBlock stmt_block(Stmt **stmts, int num_stmts) {
    return (StmtBlock){
        .stmts = ast_memdup(stmts, num_stmts * sizeof(*stmts)),
        .num_stmts = num_stmts,
    };
}

Stmt *stmt_brace_block(StmtBlock block) {
    Stmt *stmt = stmt_alloc(STMT_BRACE_BLOCK);
    stmt->block = block;
    return stmt;
}

Stmt *stmt_expr(Expr *expr) {
    Stmt *stmt = stmt_alloc(STMT_EXPR);
    stmt->expr = expr;
    return stmt;
}

Stmt *stmt_assign(TokenKind op, Expr *left, Expr *right) {
    Stmt *stmt = stmt_alloc(STMT_ASSIGN);
    stmt->assign.op = op;
    stmt->assign.left = left;
    stmt->assign.right = right;
    return stmt;
}

Stmt *stmt_init(char *name, Expr *expr) {
    Stmt *stmt = stmt_alloc(STMT_INIT);
    stmt->init.name = name;
    stmt->init.expr = expr;
    return stmt;
}

Stmt *stmt_return(Expr *expr) {
    Stmt *stmt = stmt_alloc(STMT_RETURN);
    stmt->return_stmt.expr = expr;
    return stmt;
}

Stmt *stmt_for(Stmt *init, Expr *cond, Stmt *next, StmtBlock block) {
    Stmt *stmt = stmt_alloc(STMT_FOR);
    stmt->for_stmt.init = init;
    stmt->for_stmt.cond = cond;
    stmt->for_stmt.next = next;
    stmt->for_stmt.block = block;
    return stmt;
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

