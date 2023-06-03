Arena ast_arena;

void *ast_memdup(void *src, size_t size) {
    return arena_memdup(&ast_arena, src, size);
}

bool is_cmp_op(TokenKind op) {
    return op == TOKEN_EQ_EQ  ||
           op == TOKEN_NOT_EQ ||
           op == TOKEN_LT_EQ  ||
           op == TOKEN_GT_EQ  ||
           op == '<'          ||
           op == '>';
}

SwitchCase switch_case(Expr **exprs, int num_exprs, bool is_default, StmtBlock block) {
	return (SwitchCase) {
		.exprs = ast_memdup(exprs, num_exprs * sizeof(*exprs)),
		.num_exprs = num_exprs,
		.is_default = is_default,
		.block = block,
	};
}


// Expressions
Expr *expr_alloc(ExprKind kind, SourcePos pos) {
    Expr *expr = arena_alloc_zeroed(&ast_arena, sizeof(Expr));
    expr->kind = kind;
	expr->pos = pos;
    return expr;
}

Expr *expr_int(SourcePos pos, uint64_t val, TokenMod mod) {
    Expr *expr = expr_alloc(EXPR_INT, pos);
    expr->int_val = val;
	expr->mod = mod;
    return expr;
}

Expr *expr_float(SourcePos pos, double val, TokenMod mod) {
	Expr *expr = expr_alloc(EXPR_FLOAT, pos);
    expr->float_val = val;
    expr->mod = mod;
    return expr;
}

Expr *expr_str(SourcePos pos, char *val) {
	Expr *expr = expr_alloc(EXPR_STR, pos);
    expr->str_val = val;
    return expr;
}

Expr *expr_char(SourcePos pos, char val) {
	Expr *expr = expr_alloc(EXPR_CHAR, pos);
    expr->char_val = val;
    return expr;
}

Expr *expr_bool(SourcePos pos, bool val) {
	Expr *expr = expr_alloc(EXPR_BOOL, pos);
	expr->bool_val = val;
	return expr;
}

Expr *expr_name(SourcePos pos, char *name) {
	Expr *expr = expr_alloc(EXPR_NAME, pos);
    expr->name = name;
    return expr;
}

Expr *expr_cast(SourcePos pos, Typespec *typespec, Expr *expr) {
	Expr *new_expr = expr_alloc(EXPR_CAST, pos);
    new_expr->cast.typespec = typespec;
    new_expr->cast.expr = expr;
    return new_expr;
}

Expr *expr_unary(SourcePos pos, TokenKind op, Expr *operand) {
	Expr *expr = expr_alloc(EXPR_UNARY, pos);
    expr->unary.op = op;
    expr->unary.expr = operand;
    return expr;
}

Expr *expr_binary(SourcePos pos, TokenKind op, Expr *left, Expr *right) {
	Expr *expr = expr_alloc(EXPR_BINARY, pos);
    expr->binary.op    = op;
    expr->binary.left  = left;
    expr->binary.right = right;
    return expr;
}

Expr *expr_ternary(SourcePos pos, Expr *cond, Expr *then_expr, Expr *else_expr) {
	Expr *expr = expr_alloc(EXPR_TERNARY, pos);
    expr->ternary.cond = cond;
    expr->ternary.else_expr = else_expr;
    expr->ternary.then_expr = then_expr;
    return expr;
}

Expr *expr_call(SourcePos pos, Expr *expr, Expr **args, int num_args) {
	Expr *new_expr = expr_alloc(EXPR_CALL, pos);
    new_expr->call.expr = expr;
    new_expr->call.args = ast_memdup(args, num_args * sizeof(*args));
    new_expr->call.num_args = num_args;
    return new_expr;
}

Expr *expr_index(SourcePos pos, Expr *expr, Expr *index) {
	Expr *new_expr = expr_alloc(EXPR_INDEX, pos);
    new_expr->index.expr = expr;
    new_expr->index.index = index;
    return new_expr;
}

Expr *expr_field(SourcePos pos, Expr *expr, char *field) {
	Expr *new_expr = expr_alloc(EXPR_FIELD, pos);
    new_expr->field.expr = expr;
    new_expr->field.name = field;
    return new_expr;
}

Expr *expr_compound(SourcePos pos, Typespec *typespec, Expr **args, int num_args) {
	Expr *expr = expr_alloc(EXPR_COMPOUND, pos);
    expr->compound.typespec = typespec;
    expr->compound.args = ast_memdup(args, num_args * sizeof(*args));
    expr->compound.num_args = num_args;
    return expr;
}

Expr *expr_sizeof_typespec(SourcePos pos, Typespec *typespec) {
	Expr *expr = expr_alloc(EXPR_SIZEOF_TYPE, pos);
    expr->sizeof_typespec = typespec;
    return expr;
}

Expr *expr_sizeof_expr(SourcePos pos, Expr *expr) {
	Expr *new_expr = expr_alloc(EXPR_SIZEOF_EXPR, pos);
    new_expr->sizeof_expr = expr;
    return new_expr;
}


// Types
Typespec *typespec_alloc(TypespecKind kind, SourcePos pos) {
    Typespec *typespec = arena_alloc_zeroed(&ast_arena, sizeof(Typespec));
    typespec->kind = kind;
	typespec->pos = pos;
    return typespec;
}

Typespec *typespec_name(SourcePos pos, char *name) {
    Typespec *typespec = typespec_alloc(TYPESPEC_NAME, pos);
    typespec->name = name;
    return typespec;
}

Typespec *typespec_ptr(SourcePos pos, Typespec *base) {
	Typespec *typespec = typespec_alloc(TYPESPEC_POINTER, pos);
    typespec->ptr.base = base;
    return typespec;
}

Typespec *typespec_array(SourcePos pos, Typespec *base, Expr *num_items) {
	Typespec *typespec = typespec_alloc(TYPESPEC_ARRAY, pos);
    typespec->array.base = base;
    typespec->array.num_items = num_items;
    return typespec;
}

Typespec *typespec_func(SourcePos pos, Typespec **args, int num_args, bool is_variadic, Typespec *ret) {
	Typespec *typespec = typespec_alloc(TYPESPEC_FUNC, pos);
    typespec->func.params = ast_memdup(args, num_args * sizeof(*args));
    typespec->func.num_params = num_args;
    typespec->func.is_variadic = is_variadic;
    typespec->func.ret = ret;
    return typespec;
}

// Statements
Stmt *stmt_alloc(StmtKind kind, SourcePos pos) {
    Stmt *stmt = arena_alloc_zeroed(&ast_arena, sizeof(Stmt));
    stmt->kind = kind;
	stmt->pos = pos;
    return stmt;
}

StmtBlock stmt_block(Stmt **stmts, int num_stmts) {
    return (StmtBlock){
        .stmts = ast_memdup(stmts, num_stmts * sizeof(*stmts)),
        .num_stmts = num_stmts,
    };
}

Stmt *stmt_brace_block(SourcePos pos, StmtBlock block) {
    Stmt *stmt = stmt_alloc(STMT_BRACE_BLOCK, pos);
    stmt->block = block;
    return stmt;
}

Stmt *stmt_expr(SourcePos pos, Expr *expr) {
	Stmt *stmt = stmt_alloc(STMT_EXPR, pos);
    stmt->expr = expr;
    return stmt;
}

Stmt *stmt_assign(SourcePos pos, TokenKind op, Expr *left, Expr *right) {
	Stmt *stmt = stmt_alloc(STMT_ASSIGN, pos);
    stmt->assign.op = op;
    stmt->assign.left = left;
    stmt->assign.right = right;
    return stmt;
}

Stmt *stmt_init(SourcePos pos, char *name, Typespec *typespec, Expr *expr) {
	Stmt *stmt = stmt_alloc(STMT_INIT, pos);
    stmt->init.name = name;
	stmt->init.typespec = typespec;
    stmt->init.expr = expr;
    return stmt;
}

Stmt *stmt_return(SourcePos pos, Expr *expr) {
	Stmt *stmt = stmt_alloc(STMT_RETURN, pos);
    stmt->return_stmt.expr = expr;
    return stmt;
}

Stmt *stmt_defer(SourcePos pos, Stmt *inner_stmt) {
	Stmt *stmt = stmt_alloc(STMT_DEFER, pos);
    stmt->defer.stmt = inner_stmt;
    return stmt;
}

Stmt *stmt_for(SourcePos pos, Stmt *init, Expr *cond, Stmt *next, StmtBlock block) {
	Stmt *stmt = stmt_alloc(STMT_FOR, pos);
    stmt->for_stmt.init = init;
    stmt->for_stmt.cond = cond;
    stmt->for_stmt.next = next;
    stmt->for_stmt.block = block;
    return stmt;
}

Stmt *stmt_if(SourcePos pos, Expr *cond, StmtBlock then_block, ElseIf *else_ifs, int num_else_ifs, StmtBlock else_block) {
	Stmt *stmt = stmt_alloc(STMT_IF, pos);
    stmt->if_stmt.cond = cond;
    stmt->if_stmt.then_block = then_block;
    stmt->if_stmt.else_ifs = ast_memdup(else_ifs, num_else_ifs * sizeof(*else_ifs));
    stmt->if_stmt.num_else_ifs = num_else_ifs;
    stmt->if_stmt.else_block = else_block;
    return stmt;
}

Stmt *stmt_do(SourcePos pos, Expr *cond, StmtBlock block) {
	Stmt *stmt = stmt_alloc(STMT_DO, pos);
    stmt->while_stmt.cond = cond;
    stmt->while_stmt.block = block;
    return stmt;
}

Stmt *stmt_while(SourcePos pos, Expr *cond, StmtBlock block) {
	Stmt *stmt = stmt_alloc(STMT_WHILE, pos);
    stmt->while_stmt.cond = cond;
    stmt->while_stmt.block = block;
    return stmt;
}

Stmt *stmt_switch(SourcePos pos, Expr *expr, SwitchCase *cases, int num_cases) {
	Stmt *stmt = stmt_alloc(STMT_SWITCH, pos);
    stmt->switch_stmt.expr = expr;
    stmt->switch_stmt.cases = ast_memdup(cases, num_cases * sizeof(*cases));
    stmt->switch_stmt.num_cases = num_cases;
    return stmt;
}


// Notes (annotations)
NoteList note_list(SourcePos pos, Note *notes, int num_notes) {
	return (NoteList){.pos=pos, .notes=notes, .num_notes=num_notes};
}


// Declarations
Decl *decl_alloc(DeclKind kind, SourcePos pos) {
    Decl *decl = arena_alloc_zeroed(&ast_arena, sizeof(Decl));
    decl->kind = kind;
	decl->pos = pos;
    return decl;
}

Decl *decl_const(SourcePos pos, char *name, Expr *expr) {
	Decl *decl = decl_alloc(DECL_CONST, pos);
    decl->name = name;
    decl->const_decl.expr = expr;
    return decl;
}

Decl *decl_var(SourcePos pos, char *name, Typespec *typespec, Expr *expr) {
	Decl *decl = decl_alloc(DECL_VAR, pos);
    decl->name = name;
    decl->var.typespec = typespec;
    decl->var.expr = expr;
    return decl;
}

Decl *decl_typedef(SourcePos pos, char *name, Typespec *typespec) {
	Decl *decl = decl_alloc(DECL_TYPEDEF, pos);
    decl->name = name;
    decl->typedef_decl.typespec = typespec;
    return decl;
}


Decl *decl_enum(SourcePos pos, char *name, EnumItem *items, int num_items) {
	Decl *decl = decl_alloc(DECL_ENUM, pos);
    decl->name = name;
    decl->enum_decl.items = ast_memdup(items, num_items * sizeof(*items));
    decl->enum_decl.num_items = num_items;
    return decl;
}

Decl *decl_aggregate(SourcePos pos, DeclKind kind, char *name, AggregateField *fields, int num_fields) {
    assert(kind == DECL_STRUCT || kind == DECL_UNION);
	Decl *decl = decl_alloc(kind, pos);
    decl->name = name;
    decl->aggregate.fields = ast_memdup(fields, num_fields * sizeof(*fields));
    decl->aggregate.num_fields = num_fields;
    return decl;
}

Decl *decl_func(SourcePos pos, char *name, FuncParam *params, int num_params, bool is_variadic, Typespec *ret_typespec, StmtBlock block) {
	Decl *decl = decl_alloc(DECL_FUNC, pos);
    decl->name = name;
    decl->func.params = ast_memdup(params, num_params * sizeof(*params));
    decl->func.num_params = num_params;
    decl->func.is_variadic = is_variadic;
    decl->func.ret_typespec = ret_typespec;
    decl->func.block = block;
    return decl;
}
