#define MAX_LOCAL_SYMS 2048

typedef struct Package Package;

typedef union {
	bool b;
	char c;
	int8_t sc;
	uint8_t uc;
	int16_t s;
	uint16_t us;
	int32_t i;
	uint32_t ui;
	int32_t l;
	uint32_t ul;
	int64_t ll;
	uint64_t ull;
	float f;
	double d;
	void *p;
} Val;

typedef struct {
    Type *type;
    bool is_const;
    bool is_lvalue;
    Val val;
} ResolvedExpr;

typedef enum {
    SYM_NONE,
    SYM_VAR,
    SYM_CONST,
    SYM_FUNC,
    SYM_TYPE,
    SYM_ENUM_CONST,
} SymKind;

typedef enum {
    SYM_UNRESOLVED,
    SYM_RESOLVING,
    SYM_RESOLVED,
} SymState;


// struct Sym is typedefed to Sym in type.c
struct Sym {
    char *name;
	char *external_name;
    SymKind kind;
    SymState state;
    Decl *decl;
    Type *type;
    Val val;
	Package *package;
	bool reachable;
};

struct Package {
	char *external_name;
	char *path;
	char full_path[MAX_PATH];
	Map syms_map;
	BUF(Sym **syms);
	BUF(Decl **decls);
	BUF(Decl **directives);
}; 

Map package_map;
BUF(Package **packages);
Package *current_package;
Package *builtin_package;

BUF(Sym **ordered_syms);
BUF(Sym **reachable_syms);
Map reachable_syms_map;

Sym *local_syms[MAX_LOCAL_SYMS];
Sym **local_syms_end = local_syms;

// set upon entering a new local scope and reset upon exit
// used to issue errors for defer statements that follow a scope exit stmt
Stmt *scope_exit_stmt;

Arena resolve_arena;
ResolvedExpr resolved_null = {0};

Sym *get_package_sym(Package *package, char *name) {
    return map_get(&package->syms_map, name);
}

Package *enter_package(Package *package) {
	Package *prev_package = current_package;
	current_package = package;
	return prev_package;
}

void leave_package(Package *old_package) {
	current_package = old_package;
}


bool is_null_ptr(ResolvedExpr operand) {
	return operand.type->kind == TYPE_PTR && operand.is_const && operand.val.p == 0;
}

Sym *sym_alloc(char *name, SymKind kind) {
    Sym *sym = arena_alloc_zeroed(&resolve_arena, sizeof(Sym));
    sym->name = name;
    sym->kind = kind;
	sym->package = current_package;
    return sym;
}

Sym *sym_decl(Decl *decl) {
    SymKind kind = SYM_NONE;
    switch (decl->kind) {
    case DECL_TYPEDEF:
    case DECL_STRUCT:
    case DECL_UNION:
	case DECL_ENUM:
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

Sym *sym_init(Decl *decl, Type *type) {
	assert(decl->kind == DECL_VAR);
	Sym *sym = sym_decl(decl);
	sym->type = type;
	sym->state = SYM_RESOLVED;
	return sym;
}

Sym *sym_enum_const(char *name, Decl *decl) {
	Sym *sym = sym_alloc(name, SYM_ENUM_CONST);
	sym->decl = decl;
	return sym;
}

Sym *sym_get_local(char *name) {
	for (Sym **it = local_syms_end; it > local_syms; --it) {
		Sym *sym = it[-1];
		if (sym->name == name) return sym;
	}
	return NULL;
}

Sym *sym_get(char *name) {
	// first check symbols in local scopes
	Sym *sym = sym_get_local(name);
	if (sym) return sym;
	// then check package symbols
	return get_package_sym(current_package, name);
}

void sym_global_put(char *name, Sym *sym) {
	Sym *old_sym = map_get(&current_package->syms_map, name);
	if (old_sym) {
		if (sym == old_sym) return;
		SourcePos pos = sym->decl ? sym->decl->pos : pos_builtin;
		semantic_error(pos, "duplicate definition of global symbol '%s'.", name);
		if (old_sym->decl) {
			print_note(old_sym->decl->pos, "previous definition of '%s'", name);
		}
	}
	map_put(&current_package->syms_map, name, sym);
	da_push(current_package->syms, sym);
}

bool is_local_sym(Sym *sym) {
	return sym_get_local(sym->name) != NULL;
}

bool name_in_local_scope(char *name, Sym **scope_start) {
	for (Sym **it = local_syms_end; it > scope_start; --it) {
		Sym *sym = it[-1];
		if (sym->name == name) return true;
	}
	return false;
}

void sym_put_decl(Decl *decl) {
	Sym *sym = sym_decl(decl);
	sym_global_put(sym->name, sym);

	if (decl->kind == DECL_ENUM) {
		EnumItem *items = decl->enum_decl.items;
		for (int i = 0; i < decl->enum_decl.num_items; ++i) {
			Sym *s = sym_enum_const(items[i].name, decl);
			sym_global_put(s->name, s);
		}
	}
}

// currently used for primative types
void sym_put_type(char *name, Type *type) {
    Sym *sym = sym_type(name, type);
	type->sym = sym;
	sym_global_put(sym->name, sym);
}

void sym_put_const(char *name, Type *type, Val val) {
	Sym *sym   = sym_alloc(name, SYM_CONST);
	sym->state = SYM_RESOLVED;
	sym->type  = type;
	sym->val   = val;
	sym_global_put(sym->name, sym);
}

// initializes primative types and built-in constants
void sym_init_table(void) {
	assert(builtin_package);
	Package *old_package = enter_package(builtin_package);

	// primative types
	sym_put_type(str_intern("void"),       type_void);
	sym_put_type(str_intern("char"),       type_char);
	sym_put_type(str_intern("schar"),      type_schar);
	sym_put_type(str_intern("uchar"),      type_uchar);
	sym_put_type(str_intern("short"),      type_short);
	sym_put_type(str_intern("ushort"),     type_ushort);
	sym_put_type(str_intern("int"),        type_int);
	sym_put_type(str_intern("uint"),       type_uint);
	sym_put_type(str_intern("long"),       type_long);
	sym_put_type(str_intern("ulong"),      type_ulong);
	sym_put_type(str_intern("longlong"),   type_longlong);
	sym_put_type(str_intern("ulonglong"),  type_ulonglong);
	sym_put_type(str_intern("float"),      type_float);
	sym_put_type(str_intern("double"),     type_double);
	sym_put_type(str_intern("bool"),       type_bool);

	// built-in constants
	sym_put_const(str_intern("true"),  type_bool,           (Val){.i=1});
	sym_put_const(str_intern("false"), type_bool,           (Val){.i=0});
	sym_put_const(str_intern("NULL"),  type_ptr(type_void), (Val){.p=0});

	leave_package(old_package);
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

char *type_to_str(Type *type) {
	if (type->sym) 
		return type->sym->name;

	switch (type->kind) {
		case TYPE_PTR:
			return strf("%s*", type_to_str(type->ptr.base));
		case TYPE_ARRAY:
			return strf("%s[%d]", type_to_str(type->array.base), type->array.num_items);

		// TODO:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ENUM:
		case TYPE_FUNC:

		default:
			assert(0);
			return NULL;
	}
}

bool is_foreign_decl(Decl *decl) {
	for (int i=0; i<decl->notes.num_notes; ++i) {
		if (decl->notes.notes[i].name == name_foreign)
			return true;
	}
	return false;
}



Sym *resolve_name(char *name);
ResolvedExpr resolve_expr_expected(Expr *expr, Type *expected_type);
ResolvedExpr resolve_expr(Expr *expr);
Type *resolve_typespec(Typespec *type);


void complete_type(Type *type) {
    if (type->kind == TYPE_COMPLETING) {
		assert(type->sym && type->sym->decl);
        semantic_error(type->sym->decl->pos, "cyclic type dependency");
        return;
    } else if (type->kind != TYPE_INCOMPLETE) {
        return;
    }

	type->kind = TYPE_COMPLETING;

	assert(type->sym);
	Decl *decl = type->sym->decl;
	assert(decl);

	if (decl->kind != DECL_STRUCT && decl->kind != DECL_UNION) {
		// NOTE(shaw): not sure if this should be a semantic error (for the user)
		// but regardless having the source position is useful
		semantic_error(decl->pos, "cannot complete a type that is not a struct or union");
		return;
	}

	BUF(TypeField *type_fields) = NULL; // @LEAK
	AggregateField *decl_fields = decl->aggregate.fields;
	int num_fields = decl->aggregate.num_fields;
	
	size_t align = 1;
	for (int i=0; i<num_fields; ++i) {
		Type *field_type = resolve_typespec(decl_fields[i].typespec);

		if (field_type->kind != TYPE_PTR)
			complete_type(field_type);

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
	size_t max_size = 0;
	size_t pad = 0;
	for (int i = 0; i < num_fields; ++i) {
		size_t field_size = type_fields[i].type->size;
		max_size = MAX(max_size, field_size);
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
	type->align = align;
	if (decl->kind == DECL_STRUCT) {
		type->kind = TYPE_STRUCT;
		type->size = size;
	} else {
		type->kind = TYPE_UNION;
		type->size = max_size;
	}
	da_push(ordered_syms, type->sym);
}

Type *resolve_typespec(Typespec *typespec) {
    switch (typespec->kind) {
    case TYPESPEC_NAME: {
        Sym *sym = resolve_name(typespec->name);
		if (!sym) {
			semantic_error(typespec->pos, "unknown type %s", typespec->name);
		}
		if (sym->kind != SYM_TYPE) {
			semantic_error(typespec->pos, "%s must denote a type", typespec->name);
			return NULL;
		}
        return sym->type;
    }

	case TYPESPEC_FUNC: {
		BUF(TypeField *params) = NULL;
		for (int i = 0; i < typespec->func.num_params; ++i) {
			da_push(params, (TypeField){ .type = resolve_typespec(typespec->func.params[i]) });
		}
		Type *ret_type = resolve_typespec(typespec->func.ret);
		return type_func(params, da_len(params), typespec->func.is_variadic, ret_type);
	}

	case TYPESPEC_ARRAY: {
		Type *elem_type = resolve_typespec(typespec->array.base);
		if (typespec->array.num_items) {
			ResolvedExpr size = resolve_expr(typespec->array.num_items);
			if (!size.is_const) {
				semantic_error(typespec->pos, "array size must be a constant");
				return NULL;
			}
			return type_array(elem_type, size.val.i);
		} else {
			// TODO(shaw): i don't like that this call caches an array type with size 0 that will ultimately 
			// not be used and will take up space in the cache. could think about some kind of incomplete
			// type for arrays or maybe a separate type constructor that doesn't cache it and acts like a 
			// dummy type just to allow compound expressions to have an expected type to use, essentially 
			// just to get the array element type
			return type_array(elem_type, 0);
		}
	}

    case TYPESPEC_POINTER: {
        return type_ptr(resolve_typespec(typespec->ptr.base));
	}

    default:
        assert(0);
		return NULL;
    }
}


#define CASE(_x, _type) \
	switch (operand->type->kind) { \
	case TYPE_CHAR:      operand->val._x = (_type)operand->val.c;   break; \
	case TYPE_SCHAR:     operand->val._x = (_type)operand->val.sc;  break; \
	case TYPE_UCHAR:     operand->val._x = (_type)operand->val.uc;  break; \
	case TYPE_SHORT:     operand->val._x = (_type)operand->val.s;   break; \
	case TYPE_USHORT:    operand->val._x = (_type)operand->val.us;  break; \
	case TYPE_INT:       operand->val._x = (_type)operand->val.i;   break; \
	case TYPE_UINT:      operand->val._x = (_type)operand->val.ui;  break; \
	case TYPE_LONG:      operand->val._x = (_type)operand->val.l;   break; \
	case TYPE_ULONG:     operand->val._x = (_type)operand->val.ul;  break; \
	case TYPE_LONGLONG:  operand->val._x = (_type)operand->val.ll;  break; \
	case TYPE_ULONGLONG: operand->val._x = (_type)operand->val.ull; break; \
	case TYPE_FLOAT:     operand->val._x = (_type)operand->val.f;   break; \
	case TYPE_DOUBLE:    operand->val._x = (_type)operand->val.d;   break; \
	case TYPE_BOOL:      operand->val._x = (_type)operand->val.b;   break; \
	default: assert(0); break; \
	}

void cast_operand(ResolvedExpr *operand, Type *type) {
	if (operand->is_const) {
		switch (type->kind) {
		case TYPE_CHAR:      CASE(c,   char);     break;
		case TYPE_SCHAR:     CASE(sc,  int8_t);   break;
		case TYPE_UCHAR:     CASE(uc,  uint8_t);  break;
		case TYPE_SHORT:     CASE(s,   int16_t);  break;
		case TYPE_USHORT:    CASE(us,  uint16_t); break;
		case TYPE_INT:       CASE(i,   int32_t);  break;
		case TYPE_UINT:      CASE(ui,  uint32_t); break;
		case TYPE_LONG:      CASE(l,   int32_t);  break;
		case TYPE_ULONG:     CASE(ul,  uint32_t); break;
		case TYPE_LONGLONG:  CASE(ll,  int64_t);  break;
		case TYPE_ULONGLONG: CASE(ull, uint64_t); break;
		case TYPE_FLOAT:     CASE(f,   float);    break;
		case TYPE_DOUBLE:    CASE(d,   double);   break;
		case TYPE_BOOL:      CASE(b,   bool);     break;
		case TYPE_PTR:
			assert(is_null_ptr(*operand));
			break;
		default: 
			assert(0); 
			break;
		}
	}
	operand->type = type;
}

#undef CASE

// TODO(shaw): using c's macros for min and max values of integer types for
// now. furthermore it will be the values ONLY for the system the compiler is
// compiled on. this version for now is just to get things going.
int64_t integer_min_values[] = {
	[TYPE_BOOL]      = 0,
	[TYPE_CHAR]      = 0,
	[TYPE_SCHAR]     = SCHAR_MIN,
	[TYPE_UCHAR]     = 0,
	[TYPE_SHORT]     = SHRT_MIN,
	[TYPE_USHORT]    = 0,
	[TYPE_INT]       = INT_MIN,
	[TYPE_UINT]      = 0,
	[TYPE_LONG]      = LONG_MIN,
	[TYPE_ULONG]     = 0,
	[TYPE_LONGLONG]  = LLONG_MIN,
	[TYPE_ULONGLONG] = 0,
};

uint64_t integer_max_values[] = {
	[TYPE_BOOL]      = 1,
	[TYPE_CHAR]      = UCHAR_MAX,
	[TYPE_SCHAR]     = SCHAR_MAX,
	[TYPE_UCHAR]     = UCHAR_MAX,
	[TYPE_SHORT]     = SHRT_MAX,
	[TYPE_USHORT]    = USHRT_MAX,
	[TYPE_INT]       = INT_MAX,
	[TYPE_UINT]      = UINT_MAX,
	[TYPE_LONG]      = LONG_MAX,
	[TYPE_ULONG]     = ULONG_MAX,
	[TYPE_LONGLONG]  = LLONG_MAX,
	[TYPE_ULONGLONG] = ULLONG_MAX,
};

int integer_conversion_ranks[] = {
	[TYPE_BOOL]      = 1,
	[TYPE_CHAR]      = 2,
	[TYPE_SCHAR]     = 2,
	[TYPE_UCHAR]     = 2,
	[TYPE_SHORT]     = 3,
	[TYPE_USHORT]    = 3,
	[TYPE_INT]       = 4,
	[TYPE_UINT]      = 4,
	[TYPE_LONG]      = 5,
	[TYPE_ULONG]     = 5,
	[TYPE_LONGLONG]  = 6,
	[TYPE_ULONGLONG] = 6,
};

void integer_promotion(ResolvedExpr *operand) {
	if (!is_integer_type(operand->type)) 
		return;
	if (integer_conversion_ranks[operand->type->kind] > integer_conversion_ranks[TYPE_INT])
		return;
	if (integer_min_values[TYPE_INT] <= integer_min_values[operand->type->kind] &&
		integer_max_values[TYPE_INT] >= integer_max_values[operand->type->kind]) 
	{
		cast_operand(operand, type_int);
	} else {
		cast_operand(operand, type_uint);
	}
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

ResolvedExpr resolved_const(Type *type, Val val) {
	assert(type);
	assert(is_scalar_type(type));

    return (ResolvedExpr){ 
        .type = type,
        .is_const = true,
        .val = val,
    };
}

int64_t eval_unary_op_signed(TokenKind op, int64_t val) {
	switch(op) {
	case '+': return +val;
	case '-': return -val;
	case '~': return ~val;
	default:
		assert(0);
		return 0;
	}
}

uint64_t eval_unary_op_unsigned(TokenKind op, uint64_t val) {
	switch(op) {
	case '+': return +val;
	case '-': return 0ull - val;
	case '~': return ~val;
	default:
		assert(0);
		return 0;
	}
}

double eval_unary_op_floating(TokenKind op, double val) {
	switch(op) {
	case '+': return +val;
	case '-': return -val;
	default:
		assert(0);
		return 0;
	}
}

Val eval_constant_unary_expr(TokenKind op, ResolvedExpr *operand) {
	Type *type = operand->type;
	ResolvedExpr dummy = *operand;
	ResolvedExpr result = {0}; 

	if (is_signed_integer_type(type)) {
		cast_operand(&dummy,  type_longlong);
		int64_t val = eval_unary_op_signed(op, dummy.val.ll);
		result = resolved_const(type_longlong, (Val){.ll=val});
	} else if (is_unsigned_integer_type(type)) {
		cast_operand(&dummy,  type_ulonglong);
		uint64_t val = eval_unary_op_unsigned(op, dummy.val.ull);
		result = resolved_const(type_ulonglong, (Val){.ull=val});
	} else if (is_floating_type(type)) {
		cast_operand(&dummy,  type_double);
		double val = eval_unary_op_floating(op, dummy.val.d);
		result = resolved_const(type_double, (Val){.d=val});
	} else {
		assert(0);
	}

	cast_operand(&result, type);

	return result.val;
}

ResolvedExpr resolve_expr_unary(Expr *expr) {
    assert(expr->kind == EXPR_UNARY);

    ResolvedExpr operand = resolve_expr(expr->unary.expr);
	ResolvedExpr result = resolved_null;

    switch (expr->unary.op) {
    case '*': {
        if (operand.type->kind == TYPE_PTR) {
			result = resolved_lvalue(operand.type->ptr.base);
        } else {
            semantic_error(expr->pos, "cannot dereference a non-pointer type");
		}
		break;
	}
    case '&': {
        if (operand.is_lvalue) {
			result = resolved_rvalue(type_ptr(operand.type));
        } else {
			semantic_error(expr->pos, "cannot take the address of a non-lvalue");
		}
		break;
	}
	case '!': {
		if (is_scalar_type(operand.type)) {
			cast_operand(&operand, type_bool);
			result = operand.is_const 
				? resolved_const(operand.type, (Val){.b = !operand.val.b}) 
				: resolved_rvalue(operand.type);
		} else {
			semantic_error(expr->pos, "operand of '!' operator must have integer, floating, or pointer type, got %s", 
				type_to_str(operand.type));
		}
		break;
	}
	case '~': {
		if (is_integer_type(operand.type)) {
			integer_promotion(&operand);
			if (operand.is_const) {
				Val val = eval_constant_unary_expr(expr->unary.op, &operand);
				result = resolved_const(operand.type, val);
			} else {
				result = resolved_rvalue(operand.type);
			}
		} else {
			semantic_error(expr->pos, "operand of '~' operator must have integer type, got %s", type_to_str(operand.type));
		}
		break;
	}
	case '+':
	case '-': {
		if (is_arithmetic_type(operand.type)) {
			integer_promotion(&operand);
			if (operand.is_const) {
				Val val = eval_constant_unary_expr(expr->unary.op, &operand);
				result = resolved_const(operand.type, val);
			} else {
				result = resolved_rvalue(operand.type);
			}
		} else {
			semantic_error(expr->pos, "operand of '%s' operator must have integer or floating type, got %s", 
				expr->unary.op, type_to_str(operand.type));
		}
		break;
	}
    default:
        assert(0);
		break;
    }

	return result;
}

ResolvedExpr resolve_expr_name(Expr *expr) {
    assert(expr->kind == EXPR_NAME);
    Sym *sym = resolve_name(expr->name);
	if (!sym) {
		semantic_error(expr->pos, "unknown symbol %s", expr->name);
	}

    if (sym->kind == SYM_VAR) 
        return resolved_lvalue(sym->type); 
    else if (sym->kind == SYM_FUNC) 
        return resolved_rvalue(sym->type);
	else if (sym->kind == SYM_CONST || sym->kind == SYM_ENUM_CONST) 
		return resolved_const(sym->type, sym->val);
    else {
		assert(sym->kind == SYM_TYPE);
		semantic_error(expr->pos, "expected variable, constant, or function but got type (%s)", expr->name);
        return resolved_null;
    }
}
ResolvedExpr resolve_expr(Expr *expr);

ResolvedExpr resolve_expr_cond(Expr *cond) {
	ResolvedExpr resolved = resolve_expr(cond);
	if (!is_scalar_type(resolved.type)) {
		semantic_error(cond->pos,
			"condition expression must have integer, floating, or pointer type; got %s",
			type_to_str(resolved.type));
	}
	return resolved;
}

void pointer_decay(ResolvedExpr *resolved) {
	assert(resolved->type->kind == TYPE_ARRAY);
	resolved->type = type_ptr(resolved->type->array.base);
	resolved->is_lvalue = false;
}

// based on the C standard, see https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1548.pdf
bool is_convertible(Type *from, Type *to) {
	if (from == to) {
		return true;
	} else if (from->kind == TYPE_PTR || to->kind == TYPE_PTR) {
		return (from->ptr.base->kind == TYPE_VOID || to->ptr.base->kind == TYPE_VOID);
	} else if (is_integer_type(from) && is_integer_type(to)) {
		return true;
	} else if (is_floating_type(from) && is_floating_type(to)) {
		return true;
	} else if (is_integer_type(from) && is_floating_type(to)) {
		return true;
	}
	return false;
}

bool convert_operand(ResolvedExpr *operand, Type *type) {
	if (is_convertible(operand->type, type)) {
		cast_operand(operand, type);
		return true;
	}	
	return false;
}

bool floating_conversion(ResolvedExpr *operand_left, ResolvedExpr *operand_right) {
	Type *left  = operand_left->type;
	Type *right = operand_right->type;

	if (left == right) return true;

	// conversion to double
	if (left->kind == TYPE_DOUBLE && right->kind != TYPE_DOUBLE) {
		if (is_convertible(right, type_double)) {
			cast_operand(operand_right, type_double);
			return true;
		} else {
			return false;	
		}
	} else if (right->kind == TYPE_DOUBLE && left->kind != TYPE_DOUBLE) {
		if (is_convertible(left, type_double)) {
			cast_operand(operand_left, type_double);
			return true;
		} else {
			return false;	
		}

	// conversion to float
	} else if (left->kind == TYPE_FLOAT && right->kind != TYPE_FLOAT) {
		if (is_convertible(right, type_float)) {
			cast_operand(operand_right, type_float);
			return true;
		} else {
			return false;
		}

	} else if (right->kind == TYPE_FLOAT && left->kind != TYPE_FLOAT) {
		if (is_convertible(left, type_float)) {
			cast_operand(operand_left, type_float);
			return true;
		} else {
			return false;	
		}
	}

	assert(0);
	return false;
}

// returns true if types are compatible and conversion is successful, else false
// see usual arithmetic conversions in the C standard, section 6.3.1.8
bool arithmetic_conversion(ResolvedExpr *left, ResolvedExpr *right) {
	assert(is_arithmetic_type(left->type) && is_arithmetic_type(right->type));

	if (is_floating_type(left->type) || is_floating_type(right->type)) {
		return floating_conversion(left, right);
	}

	integer_promotion(left);
	integer_promotion(right);

	if (left->type == right->type) {
		return true;
	} else if ((is_signed_integer_type(left->type) && is_signed_integer_type(right->type)) || 
	           (is_unsigned_integer_type(left->type) && is_unsigned_integer_type(right->type))) {
		// the type with lower rank is converted to the type with higher rank
		if (integer_conversion_ranks[left->type->kind] < integer_conversion_ranks[right->type->kind]) {
			cast_operand(left, right->type);
		} else {
			cast_operand(right, left->type);
		}
		return true;
	} else {
		ResolvedExpr *signed_operand, *unsigned_operand;
		if (is_signed_integer_type(left->type)) {
			signed_operand = left;
			unsigned_operand = right;
		} else {
			signed_operand = right;
			unsigned_operand = left;
		}

		if (integer_conversion_ranks[unsigned_operand->type->kind] >= integer_conversion_ranks[signed_operand->type->kind]) {
			cast_operand(signed_operand, unsigned_operand->type);
			return true;

		// if the operand with signed type can represent all of the values of the operand with unsigned type
		} else if (integer_min_values[signed_operand->type->kind] <= integer_min_values[unsigned_operand->type->kind] &&
				   integer_max_values[signed_operand->type->kind] >= integer_max_values[unsigned_operand->type->kind]) {
			cast_operand(unsigned_operand, signed_operand->type);
			return true;

		} else {
			Type *corresponding_unsigned_type[] = {
				[TYPE_SCHAR]     = type_uchar,
				[TYPE_SHORT]     = type_ushort,
				[TYPE_INT]       = type_uint,
				[TYPE_LONG]      = type_ulong,
				[TYPE_LONGLONG]  = type_ulonglong,
			};
			Type *type = corresponding_unsigned_type[signed_operand->type->kind];
			cast_operand(signed_operand, type);
			cast_operand(unsigned_operand, type);
			return true;
		}
	}
}

ResolvedExpr resolve_expr_int(Expr *expr) {
	assert(expr->kind == EXPR_INT);
	uint64_t int_val = expr->int_val;
	ResolvedExpr result = resolved_const(type_ulonglong, (Val){.ull = int_val});
	Type *type;

	if (expr->mod == TOKENMOD_HEX || expr->mod == TOKENMOD_BIN || expr->mod == TOKENMOD_OCT) {
		type = type_int;
		if (int_val > integer_max_values[TYPE_INT]) {
			type = type_uint;
			if (int_val > integer_max_values[TYPE_UINT]) {
				type = type_long;
				if (int_val > integer_max_values[TYPE_LONG]) {
					type = type_ulong;
					if (int_val > integer_max_values[TYPE_ULONG]) {
						type = type_longlong;
						if (int_val > integer_max_values[TYPE_LONGLONG]) {
							type = type_ulonglong;
						}
					}
				}
			}
		}
	} else {
		assert(expr->mod == TOKENMOD_NONE);
		type = type_int;
		if (int_val > integer_max_values[TYPE_INT]) {
			type = type_long;
			if (int_val > integer_max_values[TYPE_LONG]) {
				type = type_longlong;
			}
		}
	}

	cast_operand(&result, type);

	return result;
}


int64_t eval_binary_op_signed(TokenKind op, int64_t left, int64_t right) {
	switch(op) {
	case '+': return left + right;
	case '-': return left - right;
	case '*': return left * right;
	case '&': return left & right;
	case '|': return left | right;
	case '^': return left ^ right;
	case '<': return left < right;
	case '>': return left > right;
	case '/': return right == 0 ? 0 : left / right;
	case '%': return right == 0 ? 0 : left % right;
    case TOKEN_LSHIFT: return left << right;
    case TOKEN_RSHIFT: return left >> right;
    case TOKEN_EQ_EQ:  return left == right;
    case TOKEN_NOT_EQ: return left != right;
    case TOKEN_LT_EQ:  return left <= right;
    case TOKEN_GT_EQ:  return left >= right;
	default:
		assert(0);
		return 0;
	}
}

uint64_t eval_binary_op_unsigned(TokenKind op, uint64_t left, uint64_t right) {
	switch(op) {
	case '+': return left + right;
	case '-': return left - right;
	case '*': return left * right;
	case '&': return left & right;
	case '|': return left | right;
	case '^': return left ^ right;
	case '<': return left < right;
	case '>': return left > right;
	case '/': return right == 0 ? 0 : left / right;
	case '%': return right == 0 ? 0 : left % right;
    case TOKEN_LSHIFT: return left << right;
    case TOKEN_RSHIFT: return left >> right;
    case TOKEN_EQ_EQ:  return left == right;
    case TOKEN_NOT_EQ: return left != right;
    case TOKEN_LT_EQ:  return left <= right;
    case TOKEN_GT_EQ:  return left >= right;
	default:
		assert(0);
		return 0;
	}
}

double eval_binary_op_floating(TokenKind op, double left, double right) {
	switch(op) {
	case '+': return left + right;
	case '-': return left - right;
	case '*': return left * right;
	case '<': return left < right;
	case '>': return left > right;
	case '/': return right == 0 ? 0 : left / right;
    case TOKEN_EQ_EQ:  return left == right;
    case TOKEN_NOT_EQ: return left != right;
    case TOKEN_LT_EQ:  return left <= right;
    case TOKEN_GT_EQ:  return left >= right;
	default:
		assert(0);
		return 0;
	}
}

Val eval_constant_binary_expr(TokenKind op, ResolvedExpr *operand_left, ResolvedExpr *operand_right) {
	assert(operand_left->type == operand_right->type);
	Type *type = operand_left->type;
	ResolvedExpr left = *operand_left, right = *operand_right;
	ResolvedExpr result = {0}; 

	if (is_signed_integer_type(type)) {
		cast_operand(&left,  type_longlong);
		cast_operand(&right, type_longlong);
		int64_t val = eval_binary_op_signed(op, left.val.ll, right.val.ll);
		result = resolved_const(type_longlong, (Val){.ll=val});
	} else if (is_unsigned_integer_type(type)) {
		cast_operand(&left,  type_ulonglong);
		cast_operand(&right, type_ulonglong);
		uint64_t val = eval_binary_op_unsigned(op, left.val.ull, right.val.ull);
		result = resolved_const(type_ulonglong, (Val){.ull=val});
	} else if (is_floating_type(type)) {
		cast_operand(&left,  type_double);
		cast_operand(&right, type_double);
		double val = eval_binary_op_floating(op, left.val.d, right.val.d);
		result = resolved_const(type_double, (Val){.d=val});
	} else {
		assert(0);
	}

	cast_operand(&result, type);

	return result.val;
}

ResolvedExpr resolve_expr_expected(Expr *expr, Type *expected_type) {
    ResolvedExpr result = resolved_null;

    switch (expr->kind) {
    case EXPR_INT:
		result = resolve_expr_int(expr);
		break;
    case EXPR_CHAR: 
        result = resolved_const(type_char, (Val){.c=expr->char_val});
		break;
    case EXPR_FLOAT:
		if (expr->mod == TOKENMOD_DOUBLE)
			result = resolved_const(type_double, (Val){.d=(double)expr->float_val});
		else
			result = resolved_const(type_float, (Val){.f=(float)expr->float_val});
		break;
	case EXPR_BOOL:
		result = resolved_const(type_bool, (Val){.b=expr->bool_val});
		break;
	case EXPR_STR:
		//result = resolved_rvalue(type_array(type_char, strlen(expr->str_val) + 1));
		result = resolved_rvalue(type_ptr(type_char));
		break;
    case EXPR_NAME:
        result = resolve_expr_name(expr);
		break;
    case EXPR_UNARY:
        result = resolve_expr_unary(expr);
		break;
    case EXPR_SIZEOF_EXPR: {
        ResolvedExpr sizeof_expr = resolve_expr(expr->sizeof_expr);
        result = resolved_const(type_int, (Val){.i=sizeof_expr.type->size});
		break;
    }
    case EXPR_SIZEOF_TYPE: {
        Type *type = resolve_typespec(expr->sizeof_typespec);
        result = resolved_const(type_int, (Val){.i=type->size});
		break;
	}
	case EXPR_CAST: {
		Type *type = resolve_typespec(expr->cast.typespec);
		resolve_expr(expr->cast.expr);
		result = resolved_rvalue(type);
		break;
	}
	case EXPR_BINARY: {
		ResolvedExpr left  = resolve_expr(expr->binary.left);
		ResolvedExpr right = resolve_expr(expr->binary.right);
		if (!arithmetic_conversion(&left, &right)) {
			semantic_error(expr->pos, 
				"incompatible types for left and right side of binary expression, left is %s right is %s",
				type_to_str(left.type), type_to_str(right.type));
		}
		if (left.is_const && right.is_const) {
			Val val = eval_constant_binary_expr(expr->binary.op, &left, &right);
			result = resolved_const(left.type, val);
		} else {
			result = resolved_rvalue(left.type);
		}
		break;
	}

	case EXPR_TERNARY: {
		resolve_expr_cond(expr->ternary.cond);
		ResolvedExpr then_expr = resolve_expr(expr->ternary.then_expr);
		ResolvedExpr else_expr = resolve_expr(expr->ternary.else_expr);

		if (is_arithmetic_type(then_expr.type) && is_arithmetic_type(else_expr.type)) {
			ResolvedExpr dummy_left  = then_expr;
			ResolvedExpr dummy_right = else_expr;
			if (arithmetic_conversion(&dummy_left, &dummy_right)) {
				result = resolved_rvalue(dummy_left.type);
			} else {
				semantic_error(expr->pos,
					"incompatible types for branches in ternary expression, left is %s right is %s",
					type_to_str(then_expr.type), type_to_str(else_expr.type));
			}
		} else if (is_aggregate_type(then_expr.type) && is_aggregate_type(else_expr.type)) {
			if (then_expr.type == else_expr.type) {
				result = resolved_rvalue(then_expr.type);
			} else {
				semantic_error(expr->pos,
					"incompatible types for branches in ternary expression, left is %s right is %s",
					type_to_str(then_expr.type), type_to_str(else_expr.type));
			}
		} else if (then_expr.type == type_void && else_expr.type == type_void) {
			result = resolved_rvalue(type_void);
		} else if (then_expr.type->kind == TYPE_PTR && else_expr.type->kind == TYPE_PTR && 
				   then_expr.type == else_expr.type) {
			// TODO(shaw): the standard says "both operands are pointers to
			// qualified or unqualified versions of compatible types"
			result = resolved_rvalue(then_expr.type);
		} else if (then_expr.type->kind == TYPE_PTR && is_null_ptr(else_expr)) {
			result = resolved_rvalue(then_expr.type);
		} else if (is_null_ptr(then_expr) && else_expr.type->kind == TYPE_PTR) {
			result = resolved_rvalue(else_expr.type);
		} else if (then_expr.type->kind == TYPE_PTR && else_expr.type == type_ptr(type_void)) {
			result = resolved_rvalue(else_expr.type);
		} else if (then_expr.type == type_ptr(type_void) && else_expr.type->kind == TYPE_PTR) {
			result = resolved_rvalue(then_expr.type);
		} else {
			semantic_error(expr->pos,
				"invalid types for branches in ternary expression, left is %s right is %s",
				type_to_str(then_expr.type), type_to_str(else_expr.type));
		}
		break;
	}

	case EXPR_CALL: {
		ResolvedExpr resolved = resolve_expr(expr->call.expr);
		if (resolved.type->kind != TYPE_FUNC) {
			semantic_error(expr->pos, "attempting to call %s which is not a function", expr->call.expr->name);
			result = resolved_null;
			break;
		}
		Type *type = resolved.type;

		// check that num of args in call match the function signature
		if (expr->call.num_args < type->func.num_params) {
			semantic_error(expr->pos, "not enough arguments passed to function %s", type_to_str(type));
			result = resolved_null;
			break;
		} else if (expr->call.num_args > type->func.num_params) {
			// only allowed if variadic function
			if (!type->func.is_variadic) {
				semantic_error(expr->pos, "too many arguments passed to function %s", type_to_str(type));
				result = resolved_null;
				break;
			}
		}
		// resolve call arguments and type check against function parameters
		for (int i = 0; i < expr->call.num_args; ++i) {
			TypeField param = type->func.params[i];
			Expr *arg_expr = expr->call.args[i];
			ResolvedExpr arg = resolve_expr_expected(arg_expr, param.type);
			if (arg.type->kind == TYPE_ARRAY && arg_expr->kind != EXPR_COMPOUND) {
				pointer_decay(&arg);
			}
			if (i < type->func.num_params) {
				if (arg.type != param.type) { 
					if (!convert_operand(&arg, param.type)) {
						semantic_error(expr->pos, 
							"type mismatch in function call: expected %s for parameter %s, got %s", 
							type_to_str(param.type), param.name, type_to_str(arg.type));
						continue;
					}
				}
				expr->call.args[i]->type = param.type;
			} else {
				assert(type->func.is_variadic);
				// cannot typecheck var args, so do nothing
			}
		}

		result = resolved_rvalue(type->func.ret);
		break;
	}
	
	case EXPR_INDEX: {
		ResolvedExpr resolved_expr  = resolve_expr(expr->index.expr);
		ResolvedExpr index = resolve_expr(expr->index.index);

		if (!is_integer_type(index.type)) {
			semantic_error(expr->pos, "index expression must have integer type, got %s",
				type_to_str(index.type));
			result = resolved_null;
			break;
		}

		Type *type = resolved_expr.type;
		if (type->kind == TYPE_ARRAY) {
			result = resolved_lvalue(type->array.base);
		} else if (type->kind == TYPE_PTR) {
			result = resolved_lvalue(type->ptr.base);
		} else {
			semantic_error(expr->pos, "attempting to index a non array or pointer type");
			result = resolved_null;
		}
		break;
	}

	case EXPR_FIELD: {
		ResolvedExpr operand = resolve_expr(expr->field.expr);
		Type *type = operand.type;
		if (operand.type->kind == TYPE_PTR) {
			type = operand.type->ptr.base;
		}
		complete_type(type);
		if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION) {
			semantic_error(expr->pos, "attempting to access a field of a non struct or union");
		}

		TypeField *fields = type->aggregate.fields;
		int num_fields =  type->aggregate.num_fields;
		bool found = false;
		for (int i = 0; i < num_fields; ++i) {
			if (expr->field.name == fields[i].name) {
				result = resolved_lvalue(fields[i].type);
				found = true; 
				break;
			}
		}
		if (!found) {
			semantic_error(expr->pos, "%s is not a field of %s", expr->field.name, type_to_str(type));
		}
		break;
	}

	case EXPR_COMPOUND: {
		Typespec *typespec = expr->compound.typespec;
		if (!typespec && !expected_type) {
			semantic_error(expr->pos, "compound literal is missing a type specification in a context where its type cannot be inferred");
			break;
		}

		Expr **args = expr->compound.args;
		int num_args = expr->compound.num_args;
		Type *type = NULL;

		bool is_implicit_sized_array = 
			(typespec && typespec->kind == TYPESPEC_ARRAY && !typespec->array.num_items) ||
			(expected_type && expected_type->kind == TYPE_ARRAY && !expected_type->array.num_items);
		if (is_implicit_sized_array) {
			Type *elem_type = typespec 
				? resolve_typespec(typespec->array.base) 
				: expected_type->array.base;
			type = type_array(elem_type, num_args);
			// TODO(shaw): handle initializers with explicit indices
			// i.e. this case: {1,2,3,4, [10]=69}  size is 11 here
		} else {
			type = typespec ? resolve_typespec(typespec) : expected_type;
		}

		complete_type(type);

		if (type->kind == TYPE_ARRAY) {
			for (int i = 0; i < num_args; ++i) {
				ResolvedExpr arg = resolve_expr_expected(args[i], type->array.base);
				(void)arg;
			}
		} else {
			assert(type->kind == TYPE_STRUCT || type->kind == TYPE_UNION);
			for (int i = 0; i < num_args; ++i) {
				Type *field_type = type->aggregate.fields[i].type;
				ResolvedExpr arg = resolve_expr_expected(args[i], field_type);

				if (arg.type->kind == TYPE_ARRAY && field_type->kind == TYPE_PTR) {
					pointer_decay(&arg);
				}

				if (arg.type != field_type) {
					if (!convert_operand(&arg, field_type)) {
						semantic_error(expr->pos,
							"invalid argument type in compound expression: expected %s for field '%s' in %s, got %s",
							type_to_str(field_type), type->aggregate.fields[i].name, type_to_str(type), type_to_str(arg.type)); 
						continue;
					}	
				}
				args[i]->type = field_type;
			}
		}

		result = resolved_lvalue(type);
		break;
	}

    default:
        assert(0);
        break;
    }

    if (result.type) {
        assert(!expr->type || expr->type == result.type);
        expr->type = result.type;
    }

    return result;
}

ResolvedExpr resolve_expr(Expr *expr) {
	return resolve_expr_expected(expr, NULL);
}

bool stmt_illegal_in_defer(Stmt *stmt) {
	return stmt->kind == STMT_RETURN || stmt->kind == STMT_BREAK || stmt->kind == STMT_CONTINUE;
}

void resolve_stmt_block(StmtBlock block, Type *expected_ret_type);

// TODO(shaw): eventually, resolve_stmt can return a struct that contains some 
// ancillary data like control flow info
void resolve_stmt(Sym **scope_start, Stmt *stmt, Type *expected_ret_type) {
	assert(stmt);
	switch (stmt->kind) {
		case STMT_CONTINUE:
		case STMT_BREAK:
			scope_exit_stmt = stmt;
			break;
		case STMT_EXPR:
			resolve_expr(stmt->expr);
			break;
		case STMT_BRACE_BLOCK:
			resolve_stmt_block(stmt->block, expected_ret_type);
			break;
		case STMT_DO:
		case STMT_WHILE:
			resolve_expr_cond(stmt->while_stmt.cond);
			resolve_stmt_block(stmt->while_stmt.block, expected_ret_type);
			break;

		case STMT_RETURN: {
			scope_exit_stmt = stmt;
			if (!stmt->return_stmt.expr) {
				if (expected_ret_type && expected_ret_type != type_void) {
					semantic_error(stmt->pos, "expected return type %s, got no value", 
						type_to_str(expected_ret_type));
				}
				break;
			}
			ResolvedExpr resolved = resolve_expr(stmt->return_stmt.expr);
			if (resolved.type != expected_ret_type) {
				semantic_error(stmt->pos, "expected return type %s, got %s", 
					type_to_str(expected_ret_type), type_to_str(resolved.type));
			}
			break;
		}
		case STMT_DEFER: {
			if (scope_exit_stmt) {
				semantic_error(stmt->pos, "defer statement cannot follow return, break, or continue in the current scope");
			}
			Stmt *inner_stmt = stmt->defer.stmt;
			if (inner_stmt->kind == STMT_BRACE_BLOCK) {
				Sym **scope_start = sym_enter_scope();
				for (int i = 0; i < inner_stmt->block.num_stmts; ++i) {
					Stmt *block_stmt = inner_stmt->block.stmts[i];
					if (stmt_illegal_in_defer(block_stmt)) {
						semantic_error(block_stmt->pos,
							"illegal statement in defer block; return, break, and continue are not allowed");
						continue;
					}
					resolve_stmt(scope_start, block_stmt, expected_ret_type);
				}
				sym_leave_scope(scope_start);
			} else if (stmt_illegal_in_defer(inner_stmt)) {
				semantic_error(inner_stmt->pos,
					"illegal statement in defer; return, break, and continue are not allowed");
			} else {
				resolve_stmt(scope_start, inner_stmt, expected_ret_type);
			}
			break;
		}

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

		case STMT_FOR: {
			Sym **for_scope_start = sym_enter_scope();
			if (stmt->for_stmt.init) resolve_stmt(for_scope_start, stmt->for_stmt.init, expected_ret_type);
			if (stmt->for_stmt.cond) {
				resolve_expr_cond(stmt->for_stmt.cond);
			}
			if (stmt->for_stmt.next) resolve_stmt(for_scope_start, stmt->for_stmt.next, expected_ret_type);
			resolve_stmt_block(stmt->for_stmt.block, expected_ret_type);
			sym_leave_scope(for_scope_start);
			break;
		}

		case STMT_SWITCH: {
			ResolvedExpr expr = resolve_expr(stmt->switch_stmt.expr);
			if (!is_integer_type(expr.type)) {
				semantic_error(stmt->pos, "switch expression must have integer type, got %s", 
					type_to_str(expr.type));
			}
			int num_cases = stmt->switch_stmt.num_cases;
			SwitchCase *cases = stmt->switch_stmt.cases;
			for (int i = 0; i < num_cases; ++i) {
				SwitchCase c = cases[i];
				if (!c.is_default) {
					for (int j = 0; j < c.num_exprs; ++j) {
						Expr *case_expr = c.exprs[j];
						ResolvedExpr resolved = resolve_expr(case_expr);
						if (!is_integer_type(resolved.type)) {
							semantic_error(case_expr->pos, "case expression must have integer type, got %s", 
								type_to_str(resolved.type));
						}
					}
				}
				resolve_stmt_block(c.block, expected_ret_type);
			}
			break;
		}
		
		case STMT_ASSIGN: {
			ResolvedExpr left = resolve_expr(stmt->assign.left);
			if (stmt->assign.right) {
				ResolvedExpr right = resolve_expr(stmt->assign.right);

				if (right.type->kind == TYPE_ARRAY && stmt->assign.right->kind != EXPR_COMPOUND) {
					pointer_decay(&right);
				}
				if (left.type != right.type) {
					if (!convert_operand(&right, left.type)) {
						semantic_error(stmt->pos,
							"type mismatch in assignment statement: left type is %s, right type is %s",
							type_to_str(left.type), type_to_str(right.type));
						break;
					}
				}
				// update the expression type to account for pointer decay and conversion
				stmt->assign.right->type = right.type;
			} 
			break;
		}

		case STMT_INIT: {
			// redeclaration in the current scope is an error
			Sym *shadow = sym_get(stmt->init.name);
			if (name_in_local_scope(stmt->init.name, scope_start)) {
				semantic_error(stmt->pos, "redeclaration of variable '%s'", stmt->init.name);
				assert(shadow->name && shadow->type);
				print_note(shadow->decl->pos, "previous declaration of '%s' with type '%s'",
					shadow->name, type_to_str(shadow->type));
				break;

			// shadowing a variable name is an error
			} else if (shadow) {
				semantic_error(stmt->pos, "shadowing variable name '%s'", stmt->init.name);
				assert(shadow->name && shadow->type);
				print_note(shadow->decl->pos, "previous declaration of '%s' with type '%s'",
					shadow->name, type_to_str(shadow->type));
			}

			Type *type = NULL;
			if (stmt->init.typespec) 
				type = resolve_typespec(stmt->init.typespec);
			if (stmt->init.expr) {
				ResolvedExpr resolved = resolve_expr_expected(stmt->init.expr, type);
				if (resolved.type->kind == TYPE_ARRAY && stmt->init.expr->kind != EXPR_COMPOUND) {
					pointer_decay(&resolved);
				}

				if (!type) {
					type = resolved.type;
				} else if (type->kind == TYPE_ARRAY && type->array.num_items == 0) {
					assert(resolved.type->kind == TYPE_ARRAY && resolved.type->array.num_items > 0);
					type = resolved.type;
				} else if (resolved.type != type) {
					if (!convert_operand(&resolved, type)) {
						semantic_error(stmt->pos, 
							"type mismatch in init statement: specified type is %s, expression type is %s",
							type_to_str(type), type_to_str(resolved.type));
						break;
					}
				}
				// update the expression type to account for pointer decay and conversion
				stmt->init.expr->type = type;
			}
			// NOTE(shaw): the decl_var used here seems a bit hacky, however the declaration allows us 
			// to get a SourcePos from the symbol table which is used for error messages about variable 
			// redeclaration to point out where in the file the previous declaration occured.
			Decl *decl = decl_var(stmt->pos, stmt->init.name, stmt->init.typespec, stmt->init.expr);
			Sym *sym = sym_init(decl, type);
			sym_push_scoped(sym);
			break;
		}

		default:
			assert(0);
			break;
	}
}

void resolve_stmt_block(StmtBlock block, Type *expected_ret_type) {
	Stmt *prev_scope_exit_stmt = scope_exit_stmt;
	scope_exit_stmt = NULL;
	Sym **scope_start = sym_enter_scope();
	for (int i = 0; i < block.num_stmts; ++i) {
		resolve_stmt(scope_start, block.stmts[i], expected_ret_type);
	}
	sym_leave_scope(scope_start);
	scope_exit_stmt = prev_scope_exit_stmt;
}

Type *resolve_decl_const(Decl *decl, Val *val) {
    assert(decl->kind == DECL_CONST);
    ResolvedExpr resolved = resolve_expr(decl->const_decl.expr);
    if (!resolved.is_const) {
        semantic_error(decl->pos, "right hand side of const declaration is not a constant");
        return NULL;
    }
    *val = resolved.val;
    return resolved.type;
}

Type *resolve_decl_var(Decl *decl) {
    assert(decl->kind == DECL_VAR);
	Type *type = NULL;
	if (decl->var.typespec)
		type = resolve_typespec(decl->var.typespec);
    if (decl->var.expr) {
        ResolvedExpr resolved = resolve_expr_expected(decl->var.expr, type);
		if (resolved.type->kind == TYPE_ARRAY && decl->var.expr->kind != EXPR_COMPOUND) {
			pointer_decay(&resolved);
		}

		if (!type) {
			type = resolved.type;
		} else if (type->kind == TYPE_ARRAY && type->array.num_items == 0) {
			assert(resolved.type->kind == TYPE_ARRAY && resolved.type->array.num_items > 0);
			type = resolved.type;
		} else if (resolved.type != type) {
			if (!convert_operand(&resolved, type)) {
				semantic_error(decl->pos, 
					"type mismatch in var declaration: specified type is %s, expression type is %s",
					type_to_str(type), type_to_str(resolved.type));
			}
        }
		decl->var.expr->type = type;
    }

    return type;
}

void resolve_func_body(Decl *decl, Type *type) {
	assert(decl->kind == DECL_FUNC);

	Sym **scope_start = sym_enter_scope();
	for (int i = 0; i < decl->func.num_params; ++i) {
		FuncParam param = decl->func.params[i];
		Decl *param_decl = decl_var(decl->pos, param.name, param.typespec, NULL);
		Sym *sym = sym_init(param_decl, type->func.params[i].type);
		sym_push_scoped(sym);
	}
	resolve_stmt_block(decl->func.block, type->func.ret);
	sym_leave_scope(scope_start);
}

Type *resolve_decl_func(Decl *decl) {
	assert(decl->kind == DECL_FUNC);

	BUF(TypeField *params) = NULL; // @LEAK
	int num_params = decl->func.num_params;
	for (int i = 0; i < num_params; ++i) {
		FuncParam param = decl->func.params[i];
		Type *param_type = resolve_typespec(param.typespec); 
		complete_type(param_type);
		if (param_type == type_void) {
			semantic_error(decl->pos, "function parameter '%s' type cannot be void", param.name);
		}
		da_push(params, (TypeField){ .name=param.name, .type=param_type});
	}

	Type *ret_type = type_void;
	if (decl->func.ret_typespec) {
		ret_type = resolve_typespec(decl->func.ret_typespec);
		complete_type(ret_type);
	}
    if (ret_type->kind == TYPE_ARRAY) {
        semantic_error(decl->pos, "function return type cannot be array");
    }

	return type_func(params, num_params, decl->func.is_variadic, ret_type);
}

Type *resolve_decl_type(Decl *decl) {
	if (decl->kind == DECL_ENUM) {
		EnumItem *items = decl->enum_decl.items;
		Val val = {.i=0};
		for (int i = 0; i < decl->enum_decl.num_items; ++i) {
			Sym *item_sym = sym_get(items[i].name);
			if (items[i].expr) {
				ResolvedExpr resolved = resolve_expr(items[i].expr);
				if (!resolved.is_const) {
					semantic_error(items[i].expr->pos, "enum item can only be assigned a constant value");
				}
				val = resolved.val;
			}
			item_sym->type  = type_int;
			item_sym->val.i = val.i++;
			item_sym->state = SYM_RESOLVED;
		}
		return type_enum();
	} else if (decl->kind == DECL_TYPEDEF) {
		return resolve_typespec(decl->typedef_decl.typespec);
	} else {
		assert(0);
		return NULL;
	}
}

void resolve_decl_directive(Decl *decl) {
	assert(decl->kind == DECL_DIRECTIVE);

	// only handling "foreign" directive for now
	assert(decl->name == name_foreign);
	// if (decl->name != name_foreign) return;

	for (int i=0; i<decl->directive.num_args; ++i) {
		DirectiveArg arg = decl->directive.args[i];

		assert(arg.name == name_include || arg.name == name_source);

		ResolvedExpr resolved = resolve_expr(arg.expr);
		if (resolved.type != type_ptr(type_char)) {
			semantic_error(decl->pos, "expected include value to be type char*, got %s", 
				type_to_str(resolved.type));
		}
	}
}

void resolve_sym(Sym *sym) {
	if (!sym->reachable && !is_local_sym(sym)) {
		da_push(reachable_syms, sym);
		map_put(&reachable_syms_map, sym->name, sym);
		sym->reachable = true;
	}
    if (sym->state == SYM_RESOLVED) {
        return;
    } else if (sym->state == SYM_RESOLVING) {
		semantic_error(sym->decl->pos, "cyclic dependency");
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
		sym->state = SYM_RESOLVED;
		return; // don't add to ordered_syms until the function body is resolved
    case SYM_TYPE: 
        sym->type = resolve_decl_type(sym->decl);  
		sym->type->sym = sym;
        break;
	case SYM_ENUM_CONST:
		// resolve the entire enum decl that this enum item is apart of
		resolve_sym(sym_get(sym->decl->name));
		return;
    default:
        assert(0);
        break;
    }
    sym->state = SYM_RESOLVED;
    da_push(ordered_syms, sym);
}


void complete_sym(Sym *sym) {
    assert(sym->state == SYM_RESOLVED);
    Package *old_package = enter_package(sym->package);

	if (sym->decl && !is_foreign_decl(sym->decl)) {
		if (sym->kind == SYM_TYPE) {
			complete_type(sym->type);
		} else if (sym->kind == SYM_FUNC) {
			if (!sym->decl->func.is_incomplete) {
				resolve_func_body(sym->decl, sym->type);
			}
			da_push(ordered_syms, sym);
		}
	}

    leave_package(old_package);
}

Sym *resolve_name(char *name) {
    Sym *sym = sym_get(name);
    if (!sym) return NULL;
    resolve_sym(sym);
    return sym;
}

void resolve_package(Package *package) {
	Package *old_package = enter_package(package);
	for (int i = 0; i<da_len(package->directives); ++i) {
		resolve_decl_directive(package->directives[i]);
	}
	for (int i = 0; i<da_len(package->syms); ++i) {
		Sym *sym = package->syms[i];
		if (sym->package == package) {
			resolve_sym(sym);
		}
	}
	leave_package(old_package);
}

void complete_reachable_syms(void) {
    printf("Completing reachable symbols\n");
    int prev_num_reachable = 0;
    int num_reachable = da_len(reachable_syms);
    for (int i=0; i<num_reachable; ++i) {
        complete_sym(reachable_syms[i]);

        if (i == num_reachable - 1) {
			// if calls to complete_sym() have added new reachable syms, we
			// need to update num_reachable so these new symbols get completed
			// in this loop as well
            printf("New reachable symbols:");
            for (int j = prev_num_reachable; j < num_reachable; ++j) {
                printf(" %s/%s", reachable_syms[j]->package->path, reachable_syms[j]->name);
            }
            printf("\n");
            prev_num_reachable = num_reachable;
            num_reachable = da_len(reachable_syms);
        }
    }
}
