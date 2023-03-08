typedef enum {
    SYM_UNORDERED,
    SYM_ORDERING,
    SYM_ORDERED,
} SymState;

typedef enum {
    SYM_NONE,
    SYM_VAR,
    SYM_CONST,
    SYM_TYPE,
    SYM_FUNC,
} SymKind;

typedef struct {
    char *name;
    Decl *decl;
    SymState state;
    SymKind kind;
} Sym;

BUF(Sym *global_syms);
BUF(Sym **ordered_syms);

Sym *sym_get(char *name) {
    for (int i=0; i<da_len(global_syms); ++i) {
        Sym *sym = &global_syms[i];
        if (sym->name == name) return sym;
    }
    return NULL;
}

void sym_decl(Decl *decl) {
    assert(sym_get(decl->name) == NULL);

    da_push(global_syms, (Sym){
        .name=decl->name, 
        .decl=decl, 
        .state=SYM_UNORDERED,
        /*.kind=kind*/ });
}


Sym *order_name(char *name);
void order_expr(Expr *expr);
void order_typespec(Typespec *type);

void order_typespec(Typespec *type) {
    switch (type->kind) {
    case TYPESPEC_NAME:
        order_name(type->name);
        break;
    case TYPESPEC_FUNC:
        for (int i=0; i<type->func.num_args; ++i)
            order_typespec(type->func.args[i]);
        order_typespec(type->func.ret);
        break;
    case TYPESPEC_ARRAY:
        order_typespec(type->array.elem);
        order_expr(type->array.size);
        break;
    case TYPESPEC_POINTER:
        order_typespec(type->ptr.base);
        break;
    default:
        assert(0);
        break;
    }
}

void order_expr(Expr *expr) {
    switch (expr->kind) {
    case EXPR_INT:
    case EXPR_FLOAT:
    case EXPR_STR:
            // do nothing
            break;
    case EXPR_UNARY:
        order_expr(expr->unary.expr);
        break;
    case EXPR_BINARY: 
        order_expr(expr->binary.left);
        order_expr(expr->binary.right);
        break;
    case EXPR_TERNARY:
        order_expr(expr->ternary.cond);
        order_expr(expr->ternary.then_expr);
        order_expr(expr->ternary.else_expr);
        break;
    case EXPR_NAME:
        order_name(expr->name);
        break;
    case EXPR_CAST:
        order_typespec(expr->cast.type);
        order_expr(expr->cast.expr);
        break;
    case EXPR_CALL:
        order_expr(expr->call.expr);
        for (int i=0; i<expr->call.num_args; ++i)
            order_expr(expr->call.args[i]);
        break;
    case EXPR_INDEX:
        order_expr(expr->index.expr);
        order_expr(expr->index.index);
        break;
    case EXPR_FIELD:
        order_expr(expr->field.expr);
        order_name(expr->field.name);
        break;
    case EXPR_COMPOUND:
        order_typespec(expr->compound.type);
        for (int i=0; i<expr->compound.num_args; ++i)
            order_expr(expr->compound.args[i]);
        break;
    default:
        assert(0);
        break;
    }
}

void order_decl(Decl *decl) {
    switch (decl->kind) {
    case DECL_UNION:
    case DECL_STRUCT:
        for (int i=0; i<decl->aggregate.num_fields; ++i) {
            order_name(decl->aggregate.fields[i].name);
            order_typespec(decl->aggregate.fields[i].type);
        }
        break;
    case DECL_FUNC:
        for (int i=0; i<decl->func.num_params; ++i) {
            order_name(decl->func.params[i].name);
            order_typespec(decl->func.params[i].type);
        }
        order_typespec(decl->func.ret_type);
        break;
    case DECL_VAR:
        order_typespec(decl->var.type);
        order_expr(decl->var.expr);
        break;
    case DECL_CONST:
        order_expr(decl->const_decl.expr);
        break;
    case DECL_TYPEDEF:
        order_typespec(decl->typedef_decl.type);
        break;
    default:
        assert(0);
        break;
    }
}

void order_sym(Sym *sym) {
    if (sym->state == SYM_ORDERED) {
        return;
    } else if (sym->state == SYM_ORDERING) {
        fatal("Cyclic dependency");
        return;
    } 
    assert(sym->state == SYM_UNORDERED);
    sym->state = SYM_ORDERING;
    order_decl(sym->decl);
    sym->state = SYM_ORDERED;
    da_push(ordered_syms, sym);
}


Sym *order_name(char *name) {
    Sym *sym = sym_get(name);
    if (!sym) {
        fatal("Unknown symbol: %s", name); 
        return NULL;
    }
    order_sym(sym);
    return sym;
}

void order_test(void) {
    // basic sym_get sym_push test
    /*
    char *foo = str_intern("foo");
    Decl *decl = decl_const(foo, expr_int(69));
    assert(sym_get(decl->name) == NULL);
    sym_decl(decl);
    Sym *sym = sym_get(decl->name);
    assert(sym && sym->decl == decl);
    */


    char *decls[] = {
        "const x = y;",
        "const y = 666;",
    };

    for (int i = 0; i<array_count(decls); ++i) {
        init_stream(decls[i]);
        Decl *decl = parse_decl();
        sym_decl(decl);
    }

    for (int i = 0; i<da_len(global_syms); ++i) {
        order_sym(&global_syms[i]);
    }

    for (int i = 0; i<da_len(ordered_syms); ++i) {
        print_decl(ordered_syms[i]->decl);
        printf("\n");
    }
}
