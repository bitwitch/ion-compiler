typedef struct {
    Type *type;
    bool is_const;
    bool is_lvalue;
    int64_t val;
} ResolvedExpr;

typedef enum SymKind {
    SYM_NONE,
    SYM_VAR,
    SYM_CONST,
    SYM_FUNC,
    SYM_TYPE,
    SYM_ENUM_CONST,
} SymKind;

typedef enum SymState {
    SYM_UNRESOLVED,
    SYM_RESOLVING,
    SYM_RESOLVED,
} SymState;

typedef struct Sym {
    const char *name;
    SymKind kind;
    SymState state;
    Decl *decl;
    Type *type;
    int64_t val;
} Sym;

BUF(Sym **global_syms);
BUF(Sym **ordered_syms);

Arena resolve_arena;
ResolvedExpr resolved_null = {0};

Sym *sym_alloc(char *name, SymKind kind) {
    Sym *sym = arena_alloc_zeroed(&resolve_arena, sizeof(Sym));
    sym->name = name;
    sym->kind = kind;
    return sym;
}

Sym *sym_decl(Decl *decl) {
    SymKind kind = SYM_NONE;
    switch (decl->kind) {
    case DECL_TYPEDEF:
    case DECL_STRUCT:
    case DECL_UNION:
        kind = SYM_TYPE;
        break;
    case DECL_VAR:
        kind = SYM_VAR;
        break;
    case DECL_CONST:
        kind = SYM_CONST;
        break;
    case DECL_FUNC:
        kind = SYM_FUNC;
        break;
    default:
        assert(0);
        break;
    }
    Sym *sym = sym_alloc(decl->name, kind);
    sym->decl = decl;
    sym->state = SYM_UNRESOLVED;
    sym->kind = kind;

    if (decl->kind == DECL_STRUCT || decl->kind == DECL_UNION) {
        sym->state = SYM_RESOLVED;
        sym->type = type_incomplete();
    }

    return sym;
}

Sym *sym_type(char *name, Type *type) {
    Sym *sym = sym_alloc(name, SYM_TYPE);
    sym->type = type;
    sym->state = SYM_RESOLVED;
    return sym;
}

Sym *sym_get(char *name) {
    for (int i=0; i<da_len(global_syms); ++i) {
        Sym *sym = global_syms[i];
        if (sym->name == name) return sym;
    }
    return NULL;
}


void sym_put_decl(Decl *decl) {
    assert(sym_get(decl->name) == NULL);
    Sym *sym = sym_decl(decl);
    da_push(global_syms, sym);
}

void sym_put_type(char *name, Type *type) {
    assert(sym_get(name) == NULL);
    Sym *sym = sym_type(name, type);
    da_push(global_syms, sym);
}


Sym *resolve_name(char *name);
ResolvedExpr resolve_expr(Expr *expr);
Type *resolve_typespec(Typespec *type);


void complete_type(Type *type) {
    if (type->kind == TYPE_COMPLETING) {
        fatal("Cyclic type completion");
        return;
    } else if (type->kind != TYPE_INCOMPLETE) {
        return;
    }

    assert(0);
}

Type *resolve_typespec(Typespec *typespec) {
    switch (typespec->kind) {
    case TYPESPEC_NAME:
    {
        Sym *sym = resolve_name(typespec->name);
        complete_type(sym->type);
        return sym->type;
    }
    /*
    case TYPESPEC_FUNC:
        for (int i=0; i<typespec->func.num_args; ++i)
            resolve_typespec(typespec->func.args[i]);
        resolve_typespec(typespec->func.ret);
        break;
    case TYPESPEC_ARRAY:
        resolve_typespec(typespec->array.elem);
        resolve_expr(typespec->array.size);
        break;
    */
    case TYPESPEC_POINTER:
        return type_ptr(resolve_typespec(typespec->ptr.base));
    default:
        assert(0);
        break;
    }

    assert(0);
    return NULL;
}

ResolvedExpr resolved_rvalue(Type *type) {
    return (ResolvedExpr){ .type = type };
}

ResolvedExpr resolved_lvalue(Type *type) {
    return (ResolvedExpr){ 
        .type = type,
        .is_lvalue = true,
    };
}

ResolvedExpr resolved_const(int64_t val) {
    return (ResolvedExpr){ 
        .type = type_int,
        .is_const = true,
        .val = val,
    };
}

ResolvedExpr resolve_expr_unary(Expr *expr) {
    assert(expr->kind == EXPR_UNARY);

    ResolvedExpr operand = resolve_expr(expr->unary.expr);

    switch (expr->unary.op) {
    case '*':
        if (operand.type->kind != TYPE_PTR) {
            fatal("Cannot dereference a non-pointer type");
        }
        return resolved_lvalue(operand.type->ptr.base);
    case '&':
        if (!operand.is_lvalue) {
            fatal("Cannot take the address of a non-lvalue");
        }
        return resolved_rvalue(type_ptr(operand.type));
    default:
        assert(0);
        return resolved_null;
    }
}

ResolvedExpr resolve_expr_name(Expr *expr) {
    assert(expr->kind == EXPR_NAME);
    Sym *sym = resolve_name(expr->name);

    if (sym->kind == SYM_VAR) 
        return resolved_lvalue(sym->type); 
    else if (sym->kind == SYM_CONST)
        return resolved_const(sym->val);
    else if (sym->kind == SYM_FUNC) 
        return resolved_rvalue(sym->type);
    else {
        fatal("%s must be a var or const", expr->name);
        return resolved_null;
    }
}

ResolvedExpr resolve_expr(Expr *expr) {
    ResolvedExpr resolved = {0};
    switch (expr->kind) {
    case EXPR_INT:
        return resolved_const(expr->int_val);
    case EXPR_FLOAT:
        assert(0);
        break;
    case EXPR_NAME:
        return resolve_expr_name(expr);
    case EXPR_UNARY:
        return resolve_expr_unary(expr);
    case EXPR_SIZEOF_EXPR:
    {
        ResolvedExpr sizeof_expr = resolve_expr(expr->sizeof_expr);
        return resolved_const(sizeof_expr.type->size);
    }
    case EXPR_SIZEOF_TYPE:
    {
        Type *type = resolve_typespec(expr->sizeof_type);
        return resolved_const(type->size);
    }
        /*
    case EXPR_BINARY: 
        resolve_expr(expr->binary.left);
        resolve_expr(expr->binary.right);
        break;
    case EXPR_TERNARY:
        resolve_expr(expr->ternary.cond);
        resolve_expr(expr->ternary.then_expr);
        resolve_expr(expr->ternary.else_expr);
        break;
    case EXPR_CAST:
        resolve_typespec(expr->cast.type);
        resolve_expr(expr->cast.expr);
        break;
    case EXPR_CALL:
        resolve_expr(expr->call.expr);
        for (int i=0; i<expr->call.num_args; ++i)
            resolve_expr(expr->call.args[i]);
        break;
    case EXPR_INDEX:
        resolve_expr(expr->index.expr);
        resolve_expr(expr->index.index);
        break;
    case EXPR_FIELD:
        resolve_expr(expr->field.expr);
        resolve_name(expr->field.name);
        break;
    case EXPR_COMPOUND:
        resolve_typespec(expr->compound.type);
        for (int i=0; i<expr->compound.num_args; ++i)
            resolve_expr(expr->compound.args[i]);
        break;
        */
    default:
        assert(0);
        break;
    }
    return resolved;
}

void resolve_decl(Decl *decl) {
    switch (decl->kind) {
    case DECL_UNION:
    case DECL_STRUCT:
        for (int i=0; i<decl->aggregate.num_fields; ++i) {
            resolve_typespec(decl->aggregate.fields[i].type);
        }
        break;
    case DECL_FUNC:
        for (int i=0; i<decl->func.num_params; ++i) {
            resolve_typespec(decl->func.params[i].type);
        }
        resolve_typespec(decl->func.ret_type);
        break;
    case DECL_VAR:
        resolve_typespec(decl->var.type);
        if (decl->var.expr)
            resolve_expr(decl->var.expr);
        break;
    case DECL_CONST:
        resolve_expr(decl->const_decl.expr);
        break;
    case DECL_TYPEDEF:
        resolve_typespec(decl->typedef_decl.type);
        break;
    default:
        assert(0);
        break;
    }
}

Type *resolve_decl_const(Decl *decl, int64_t *val) {
    assert(decl->kind == DECL_CONST);
    ResolvedExpr resolved = resolve_expr(decl->const_decl.expr);
    if (!resolved.is_const) {
        fatal("Right hand side of const declaration is not a constant");
        return NULL;
    }
    *val = resolved.val;
    return resolved.type;
}

Type *resolve_decl_var(Decl *decl) {
    assert(decl->kind == DECL_VAR);
    // TODO (optional type here, can potentially be inferred)
    Type *type = resolve_typespec(decl->var.type);
    if (decl->var.expr) {
        ResolvedExpr resolved = resolve_expr(decl->var.expr);
        if (resolved.type != type) {
            fatal("Type mismatch in var declaration");
            return NULL;
        }
    }
    return type;
}

void resolve_sym(Sym *sym) {
    if (sym->state == SYM_RESOLVED) {
        return;
    } else if (sym->state == SYM_RESOLVING) {
        fatal("Cyclic dependency");
        return;
    } 
    assert(sym->state == SYM_UNRESOLVED);
    sym->state = SYM_RESOLVING;

    switch (sym->kind) {
    case SYM_VAR:   
        sym->type = resolve_decl_var(sym->decl);
        break;
    case SYM_CONST: 
        sym->type = resolve_decl_const(sym->decl, &sym->val); 
        break;
        /*
    case SYM_FUNC:  
        sym->type = resolve_decl_func(sym->decl);  
        break;
    case SYM_TYPE: 
        sym->type =resolve_decl_type(sym->decl);  
        break;
        */
    default:
        assert(0);
        break;
    }
    sym->state = SYM_RESOLVED;
    da_push(ordered_syms, sym);
}


void complete_sym(Sym *sym) {
    resolve_sym(sym);
    if (sym->kind == SYM_TYPE)
        complete_type(sym->type);
}

Sym *resolve_name(char *name) {
    Sym *sym = sym_get(name);
    if (!sym) {
        fatal("Unknown symbol: %s", name); 
        return NULL;
    }
    resolve_sym(sym);
    return sym;
}



void resolve_test(void) {
    sym_put_type(str_intern("int"), type_int);
    sym_put_type(str_intern("float"), type_float);
    sym_put_type(str_intern("char"), type_char);

    char *decls[] = {
        "const p = sizeof(*j)",
        "const q = sizeof(:int)",
        "var y: int = x;",
        "var x: int = 69;",
        "var j: int* = &x",

        /*
        "const x = y;",
        "const y = 666;",
        "var m: Mesh;",
        "struct Mesh { data: u8*; }",
        "typedef u8 = char;",
        "func f1(start: Vec3, end: Vec3): Vec3 { }",
        "struct Vec3 { x: int, y: int, z: int }",

        "const y = sizeof(*x)",
        "var x: B*",
        "struct B { x: int; }",
        */
    };

    for (size_t i = 0; i<array_count(decls); ++i) {
        init_stream(decls[i]);
        Decl *decl = parse_decl();
        sym_put_decl(decl);
    }

    for (int i = 0; i<da_len(global_syms); ++i) {
        complete_sym(global_syms[i]);
    }

    for (int i = 0; i<da_len(ordered_syms); ++i) {
        Sym *sym = ordered_syms[i];
        print_decl(sym->decl);
        printf("\n");
    }
}




