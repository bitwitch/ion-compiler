typedef struct Type Type;

typedef enum {
    TYPE_NONE,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_PTR,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ENUM,
    TYPE_FUNC,
    TYPE_CONST,
} TypeKind;

typedef struct {
    char *name;
    Type *type;
} TypeField;

struct Type {
    TypeKind kind;
    union {
        struct {
            Type *base;
        } ptr;
        struct {
            Type *base;
            int num_items;
        } array;
        struct {
            TypeField *fields;
            int num_fields;
        } aggregate;
        struct {
            TypeField *params;
            int num_params;
            Type *ret;
        } func;
    };
};


// symbol

typedef enum {
    SYM_UNRESOLVED,
    SYM_RESOLVING,
    SYM_RESOLVED,
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


Arena resolve_arena;
BUF(Sym *global_syms);
BUF(Sym **ordered_syms);
BUF(Type **cached_ptr_types);
BUF(Type **cached_array_types);
BUF(Type **cached_func_types);
Type type_int_val = { .kind = TYPE_INT };
Type type_float_val = { .kind = TYPE_FLOAT };
Type *type_int = &type_int_val;
Type *type_float = &type_float_val;



Type *type_alloc(TypeKind kind) {
    Type *t = arena_alloc_zeroed(&resolve_arena, sizeof(Type));
    t->kind = kind;
    return t;
}

Type *type_ptr(Type *base) {
    for (int i=0; i<da_len(cached_ptr_types); ++i) {
        Type *cached = cached_ptr_types[i];
        if (cached->ptr.base == base)
            return cached;
    }

    Type *t = type_alloc(TYPE_PTR);
    t->ptr.base = base;
    da_push(cached_ptr_types, t);
    return t;
}

Type *type_array(Type *base, int num_items) {
    for (int i=0; i<da_len(cached_array_types); ++i) {
        Type *cached = cached_array_types[i];
        if (cached->array.base == base && cached->array.num_items == num_items)
            return cached;
    }

    Type *t = type_alloc(TYPE_ARRAY);
    t->array.base = base;
    t->array.num_items = num_items;
    da_push(cached_array_types, t);
    return t;
}

Type *type_func(TypeField *params, int num_params, Type *ret) {
    for (int i=0; i<da_len(cached_func_types); ++i) {
        Type *cached = cached_func_types[i];
        if (cached->func.num_params == num_params && cached->func.ret == ret) {
            for (int j=0; j<cached->func.num_params; ++j) {
                if (params[j].type != cached->func.params[j].type) {
                    goto new_func_type;
                }
            }
            return cached;
        }
    }

new_func_type:
    ; // allow the declaration on the next line
    Type *t = type_alloc(TYPE_FUNC);
    t->func.params = arena_memdup(&resolve_arena, params, num_params * sizeof(*params));
    t->func.num_params = num_params;
    t->func.ret = ret;
    da_push(cached_func_types, t);
    return t;
}



Sym *sym_get(char *name) {
    for (int i=0; i<da_len(global_syms); ++i) {
        Sym *sym = &global_syms[i];
        if (sym->name == name) return sym;
    }
    return NULL;
}

void sym_put(Decl *decl) {
    assert(sym_get(decl->name) == NULL);
    da_push(global_syms, (Sym){.name=decl->name, .decl=decl, .state=SYM_UNRESOLVED});
}


Sym *resolve_name(char *name);

void resolve_const_expr(Expr *expr) {
    switch (expr->kind) {
    case EXPR_INT:
    case EXPR_FLOAT:
        break;
    case EXPR_NAME:
    {
        Sym *sym = resolve_name(expr->name);
        (void)sym;
        break;
    }
    default: 
        assert(0);
        break;
    }
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
    /*
    case SYM_TYPE:
        break;
    case SYM_FUNC:
        break;
    case SYM_VAR:
        break;
    */
    case SYM_CONST:
        resolve_const_expr(sym->decl->const_decl.expr);
        break;
    default:
        assert(0);
        break;
    }

    da_push(ordered_syms, sym);
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

void resolve_type_intern_test(void) {
    // primatives
    Type *int_ptr = type_ptr(type_int);
    assert(type_ptr(type_int) == int_ptr);
    Type *float_ptr = type_ptr(type_float);
    assert(type_ptr(type_float) == float_ptr);
    assert(float_ptr != int_ptr);

    // functions
    TypeField params_one[] = {
        {.name = str_intern("a"), .type = type_int }, 
        {.name = str_intern("b"), .type = int_ptr },
    };
    TypeField params_two[] = {
        {.name = str_intern("x"), .type = type_int }, 
        {.name = str_intern("y"), .type = int_ptr },
    };
    Type *func_one = type_func(params_one, array_count(params_one), type_int);
    Type *func_two = type_func(params_two, array_count(params_two), type_int);
    Type *func_three = type_func(NULL, 0, type_int);
    assert(func_one == func_two);
    assert(func_one != func_three);
    assert(func_two != func_three);

    // arrays
    Type *array_sixteen_int = type_array(type_int, 16);
    Type *array_twelve_int = type_array(type_int, 12);
    Type *array_sixteen_float = type_array(type_float, 16);
    assert(type_array(type_int, 16) == array_sixteen_int);
    assert(type_array(type_int, 12) == array_twelve_int);
    assert(type_array(type_float, 16) == array_sixteen_float);
    assert(array_sixteen_int != array_twelve_int);
    assert(array_sixteen_int != array_sixteen_float);
}

void resolve_test(void) {
    { // basic sym_get sym_push test
        char *x = str_intern("x");
        Decl *decl = decl_const(x, expr_int(69));
        assert(sym_get(decl->name) == NULL);
        sym_put(decl);
        Sym *sym = sym_get(decl->name);
        assert(sym && sym->decl == decl);
    }

    resolve_type_intern_test();


    /*
    { // basic reordering
        char *x = str_intern("x");
        char *y = str_intern("y");

        Decl *decls[] = {
            decl_const(x, expr_name(y)),
            decl_const(y, expr_int(666)),
        };

        for (int i=0; i<array_count(decls); ++i) {
            sym_put(decl);
        }
        for (int i=0; i<da_len(global_syms); ++i) {
            resolve_sym(global_syms[i]);
        }
    }
    */
}
