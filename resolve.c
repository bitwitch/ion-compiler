typedef enum {
    SYM_UNRESOLVED,
    SYM_RESOLVING,
    SYM_RESOLVED,
} SymState;

typedef struct {
    char *name;
    Decl *decl;
    SymState state;
} Sym;


Sym *symbols = NULL;

Sym *sym_get(char *name) {
    for (int i=0; i<da_len(symbols); ++i) {
        Sym *sym = &symbols[i];
        if (sym->name == name) return sym;
    }
    return NULL;
}

void sym_put(Decl *decl) {
    assert(sym_get(decl->name) == NULL);
    da_push(symbols, (Sym){.name=decl->name, .decl=decl, .state=SYM_UNRESOLVED});
}

void resolve_test(void) {
    char *x = str_intern("x");
    Decl *decl = decl_const(x, expr_int(69));
    assert(sym_get(decl->name) == NULL);
    sym_put(decl);
    Sym *sym = sym_get(decl->name);
    assert(sym && sym->decl == decl);
}
