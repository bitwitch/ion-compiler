#define MAX_LOCAL_SYMS 2048

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

// struct Sym is typedefed to Sym in type.c
struct Sym {
    const char *name;
    SymKind kind;
    SymState state;
    Decl *decl;
    Type *type;
    int64_t val;
};


BUF(Sym **global_syms);
BUF(Sym **ordered_syms);
Sym *local_syms[MAX_LOCAL_SYMS];
Sym **local_syms_end = local_syms;

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
        sym->type = type_incomplete(sym);
    }

    return sym;
}

Sym *sym_type(char *name, Type *type) {
    Sym *sym = sym_alloc(name, SYM_TYPE);
    sym->type = type;
    sym->state = SYM_RESOLVED;
    return sym;
}

// this is meant for init statements, therefore the symbol is not added to
// global_syms. it will likely be pushed to the local symbol table by the caller
Sym *sym_init(Decl *decl, Type *type) {
	assert(decl->kind == DECL_VAR);
	Sym *sym = sym_decl(decl);
	sym->type = type;
	sym->state = SYM_RESOLVED;
	return sym;
}

Sym *sym_get(char *name) {
	// first check symbols in local scopes
	for (Sym **it = local_syms_end; it > local_syms; --it) {
		Sym *sym = it[-1];
		if (sym->name == name) return sym;
	}
	// then check global symbols
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

// currently used for primative types
void sym_put_type(char *name, Type *type) {
    assert(sym_get(name) == NULL);
    Sym *sym = sym_type(name, type);
    da_push(global_syms, sym);
}

Sym **sym_enter_scope(void) {
	return local_syms_end;
}

void sym_leave_scope(Sym **scope_start) {
	local_syms_end = scope_start;
}

void sym_push_scoped(Sym *sym) {
	if (local_syms_end > local_syms + MAX_LOCAL_SYMS) {
		fatal("Too many local symbols, max is %d", MAX_LOCAL_SYMS);
	}
	*local_syms_end++ = sym;
}

Sym *resolve_name(char *name);
ResolvedExpr resolve_expr_expected(Expr *expr, Type *expected_type);
ResolvedExpr resolve_expr(Expr *expr);
Type *resolve_typespec(Typespec *type);


void complete_type(Type *type) {
    if (type->kind == TYPE_COMPLETING) {
        fatal("Cyclic type completion");
        return;
    } else if (type->kind != TYPE_INCOMPLETE) {
        return;
    }

	assert(type->sym);
	Decl *decl = type->sym->decl;
	assert(decl);

	if (decl->kind != DECL_STRUCT && decl->kind != DECL_UNION) {
		fatal("Cannot complete a type that is not a struct or union");
		return;
	}

	BUF(TypeField *type_fields) = NULL; // @LEAK
	AggregateField *decl_fields = decl->aggregate.fields;
	int num_fields = decl->aggregate.num_fields;
	
	size_t align = 1;
	for (int i=0; i<num_fields; ++i) {
		Type *field_type = resolve_typespec(decl_fields[i].type);
		align = MAX(align, field_type->align);
		da_push(type_fields, (TypeField){
			.name = decl_fields[i].name,  
			.type = field_type,
		});
	}

	// accumulate fields until adding the next one would overflow the alignment
	// add required padding 
	// for the last one, add padding if required (after loop)
	size_t accum = 0;
	size_t size = 0;
	size_t pad = 0;
	for (int i = 0; i < num_fields; ++i) {
		size_t field_size = type_fields[i].type->size;
		if (accum + field_size > align) {
			// TODO(shaw): should this padding be stored in the type somewhere??
			// or do we just recontruct the padding positions when actually laying
			// variable of this type out in memory ?
			pad = align - accum;

			size += align;
			accum = 0;
		} 
		accum += field_size;
	}
	// get padding required for last field, and update size
	pad = align - accum;
	(void)pad;
	size += align;

	type->aggregate.fields = arena_memdup(&resolve_arena, type_fields, num_fields * sizeof(*type_fields));
	type->aggregate.num_fields = num_fields;
	type->size = size;
	type->align = align;

	if (decl->kind == DECL_STRUCT)
		type->kind = TYPE_STRUCT;
	else
		type->kind = TYPE_UNION;

	da_push(ordered_syms, type->sym);
}

Type *resolve_typespec(Typespec *typespec) {
    switch (typespec->kind) {
    case TYPESPEC_NAME:
    {
        Sym *sym = resolve_name(typespec->name);
		if (sym->kind != SYM_TYPE) {
			// TODO: fatal(typespec->pos, "%s must denote a type", name);
			fatal("%s must denote a type", typespec->name);
			return NULL;
		}
        return sym->type;
    }
    /*
    case TYPESPEC_FUNC:
        for (int i=0; i<typespec->func.num_args; ++i)
            resolve_typespec(typespec->func.args[i]);
        resolve_typespec(typespec->func.ret);
        break;
	*/
	case TYPESPEC_ARRAY: {
		Type *elem_type = resolve_typespec(typespec->array.elem);
		ResolvedExpr size = resolve_expr(typespec->array.size);
		if (!size.is_const) {
			fatal("Array size must be a constant");
			return NULL;
		}
		return type_array(elem_type, size.val);
	}
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

ResolvedExpr resolve_expr_expected(Expr *expr, Type *expected_type) {
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
    case EXPR_SIZEOF_EXPR: {
        ResolvedExpr sizeof_expr = resolve_expr(expr->sizeof_expr);
        return resolved_const(sizeof_expr.type->size);
    }
    case EXPR_SIZEOF_TYPE: {
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
    */
	case EXPR_COMPOUND: {

		if (!expr->compound.type && !expected_type) {
			fatal("Compound literal is missing a type specification in a context where its type cannot be inferred.");
			break;
		}

		Expr **args = expr->compound.args;
		int num_args = expr->compound.num_args;
		
		Type *type = expr->compound.type 
			? resolve_typespec(expr->compound.type) 
			: expected_type;

		complete_type(type);

		if (type->kind == TYPE_ARRAY) {
			for (int i = 0; i < num_args; ++i) {
				ResolvedExpr arg = resolve_expr_expected(args[i], type->array.base);
				(void)arg;
			}
		} else {
			assert(type->kind == TYPE_STRUCT || type->kind == TYPE_UNION);
			for (int i = 0; i < num_args; ++i) {
				ResolvedExpr arg = resolve_expr_expected(args[i], type->aggregate.fields[i].type);
				(void)arg;
			}
		}
		// TODO(shaw): need to think more about this, it seems like a compound
		// literal should be r-value, but not totally sure
		return resolved_rvalue(type);
	}
    default:
        assert(0);
        break;
    }
    return resolved;

}

ResolvedExpr resolve_expr(Expr *expr) {
	return resolve_expr_expected(expr, NULL);
}

ResolvedExpr resolve_expr_cond(Expr *cond) {
	ResolvedExpr resolved = resolve_expr(cond);
	if (resolved.type != type_int)
		fatal("condition must have type int");
	return resolved;
}

void resolve_stmt_block(StmtBlock block, Type *expected_ret_type);

// TODO(shaw): eventually, resolve_stmt can return a struct that contains some 
// ancillary data like control flow info
void resolve_stmt(Stmt *stmt, Type *expected_ret_type) {
	assert(stmt);
	switch (stmt->kind) {
		case STMT_CONTINUE:
		case STMT_BREAK:
			// do nothing
			break;
		case STMT_RETURN: {
			if (!stmt->return_stmt.expr) break;
			ResolvedExpr resolved = resolve_expr(stmt->return_stmt.expr);
			if (resolved.type != expected_ret_type) {
				// TODO(shaw): implement a function for getting a string version of a type for printing
				// fatal("Expected return type %s, got %s", );
				fatal("return type does not match expected type");
			}
			break;
		}
		case STMT_BRACE_BLOCK:
			resolve_stmt_block(stmt->block, expected_ret_type);
			break;
		case STMT_IF: {
			resolve_expr_cond(stmt->if_stmt.cond);
			resolve_stmt_block(stmt->if_stmt.then_block, expected_ret_type);
			ElseIf *else_ifs = stmt->if_stmt.else_ifs;
			for (int i = 0; i < stmt->if_stmt.num_else_ifs; ++i) {
				resolve_expr_cond(else_ifs[i].cond);
				resolve_stmt_block(else_ifs[i].block, expected_ret_type);
			}
			resolve_stmt_block(stmt->if_stmt.else_block, expected_ret_type);
			break;
		}
		case STMT_FOR:
			if (stmt->for_stmt.init) resolve_stmt(stmt->for_stmt.init, expected_ret_type);
			if (stmt->for_stmt.cond) resolve_expr(stmt->for_stmt.cond);
			if (stmt->for_stmt.next) resolve_stmt(stmt->for_stmt.next, expected_ret_type);
			resolve_stmt_block(stmt->for_stmt.block, expected_ret_type);
			break;
		case STMT_DO:
		case STMT_WHILE: {
			resolve_expr_cond(stmt->while_stmt.cond);
			resolve_stmt_block(stmt->while_stmt.block, expected_ret_type);
			break;
		}
		/*
		case STMT_SWITCH:
			resolve_expr(stmt->switch_stmt.expr);
			int num_cases = stmt->switch_stmt.num_cases;
			SwitchCase *cases = stmt->switch_stmt.cases;
			for (int i = 0; i < num_cases; ++i) {
				
			}
		*/
		case STMT_ASSIGN:
			resolve_expr(stmt->assign.left);
			resolve_expr(stmt->assign.right);
			break;
		case STMT_INIT: {
			Type *type = NULL;
			if (stmt->init.type) 
				type = resolve_typespec(stmt->init.type);
			if (stmt->init.expr) {
				ResolvedExpr resolved = resolve_expr_expected(stmt->init.expr, type);
				type = resolved.type;
			}
			Decl *decl = decl_var(stmt->init.name, stmt->init.type, stmt->init.expr);
			Sym *sym = sym_init(decl, type);
			sym_push_scoped(sym);
			break;
		}
		case STMT_EXPR:
			resolve_expr(stmt->expr);
			break;
		default:
			assert(0);
			break;
	}
}

void resolve_stmt_block(StmtBlock block, Type *expected_ret_type) {
	Sym **scope_start = sym_enter_scope();
	for (int i = 0; i < block.num_stmts; ++i) {
		resolve_stmt(block.stmts[i], expected_ret_type);
	}
	sym_leave_scope(scope_start);
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
	Type *type = NULL;
	if (decl->var.type)
		type = resolve_typespec(decl->var.type);
    if (decl->var.expr) {
        ResolvedExpr resolved = resolve_expr_expected(decl->var.expr, type);
		if (!type) {
			type = resolved.type;
		} else if (resolved.type != type) {
            fatal("Type mismatch in var declaration");
            return NULL;
        }
    }

    return type;
}

Type *resolve_decl_func(Decl *decl) {
	assert(decl->kind == DECL_FUNC);

	BUF(TypeField *params) = NULL; // @LEAK
	int num_params = decl->func.num_params;
	for (int i = 0; i < num_params; ++i) {
		FuncParam param = decl->func.params[i];
		da_push(params, (TypeField){ .name=param.name, .type=resolve_typespec(param.type) });
	}

	Type *ret_type = resolve_typespec(decl->func.ret_type);

	// TODO(shaw): should resolving the function body happen here?
	// or lazily only when we really need to know the function body
	// similar to complete_type()
	resolve_stmt_block(decl->func.block, ret_type);

	return type_func(params, num_params, ret_type);
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
    case SYM_FUNC:  
        sym->type = resolve_decl_func(sym->decl);  
        break;
		/*
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
	// insert primative types into the symbol table at startup 
    sym_put_type(str_intern("int"), type_int);
    sym_put_type(str_intern("float"), type_float);
    sym_put_type(str_intern("char"), type_char);

    char *decls[] = {
		"var result: int = 69;",
		"func f1(start: Vec3, end: Vec3): Vec3 { result: Vec3 = {6, 6, 6}; return result; }",
		"struct Vec3 { x: int, y: int, z: int }",

		/*
		"var vec_ptr: Vec2*;",
		"var accel = Vec2{ 1, 2 };",
		"var vel: Vec2 = { 1, 2 };",
		"var pos: Vec2 = Vec2{ 6, 9 };",
		"struct Vec2 { x: int; y: int; }",
		"var vecs: Vec2[2][2] = {{{1,2},{3,4}}, {{5,6},{7,8}}};",
		
		
		"var i: int = 69;",
		"struct Vec2 { x: int; y: int*; }",
		"var vecs: Vec2[2][2] = {{{1,&i},{3,&i}}, {{5,&i},{7,&i}}};",
		
		"func f1(start: Vec3, end: Vec3): Vec3 { }",
		"struct Vec3 { x: int, y: int, z: int }",

		
        "const p = sizeof(*j);",
        "const q = sizeof(:int);",
        "var y: int = x;",
        "var x: int = 69;",
        "var j: int* = &x;",
	
        "const x = y;",
        "const y = 666;",
        "var m: Mesh;",
        "struct Mesh { data: u8*; }",
        "typedef u8 = char;",
       

        "const y = sizeof(*x)",
        "var x: B*",
        */

		/*
		// Negative (failing) tests
		"var accel = { 1, 2 };", // type cannot be inferred
		"func f2(start: int, end: int): int* { result: int = 69; return result; }",
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




