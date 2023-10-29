#define PTR_SIZE   8
#define INT_SIZE   4

typedef struct Type Type;
typedef struct Sym Sym;

typedef enum {
    TYPE_NONE,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING,
	TYPE_VOID,
    TYPE_CHAR,
    TYPE_SCHAR,
    TYPE_UCHAR,
    TYPE_SHORT,
    TYPE_USHORT,
    TYPE_INT,
    TYPE_UINT,
    TYPE_LONG,
    TYPE_ULONG,
    TYPE_LONGLONG,
    TYPE_ULONGLONG,
    TYPE_FLOAT,
    TYPE_DOUBLE,
	TYPE_BOOL,
    TYPE_PTR,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ENUM,
    TYPE_FUNC,
} TypeKind;

typedef struct {
    char *name; // field name
    Type *type; // field type
} TypeField;

struct Type {
    TypeKind kind;
	Sym *sym;
	size_t size;
	size_t align;
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
			bool is_variadic;
            Type *ret;
        } func;
    };
};

Arena type_arena;
Map cached_ptr_types;
BUF(Type **cached_array_types);
BUF(Type **cached_func_types);


// primative types
Type *type_void        = &(Type){ .kind = TYPE_VOID };
Type *type_char        = &(Type){ .kind = TYPE_CHAR,      .size = 1, .align = 1 };
Type *type_schar       = &(Type){ .kind = TYPE_SCHAR,     .size = 1, .align = 1 };
Type *type_uchar       = &(Type){ .kind = TYPE_UCHAR,     .size = 1, .align = 1 };
Type *type_short       = &(Type){ .kind = TYPE_SHORT,     .size = 2, .align = 2 };
Type *type_ushort      = &(Type){ .kind = TYPE_USHORT,    .size = 2, .align = 2 };
Type *type_int         = &(Type){ .kind = TYPE_INT,       .size = 4, .align = 4 };
Type *type_uint        = &(Type){ .kind = TYPE_UINT,      .size = 4, .align = 4 };
Type *type_long        = &(Type){ .kind = TYPE_LONG,      .size = 4, .align = 4 };
Type *type_ulong       = &(Type){ .kind = TYPE_ULONG,     .size = 4, .align = 4 };
Type *type_longlong    = &(Type){ .kind = TYPE_LONGLONG,  .size = 8, .align = 8 };
Type *type_ulonglong   = &(Type){ .kind = TYPE_ULONGLONG, .size = 8, .align = 8 };
Type *type_float       = &(Type){ .kind = TYPE_FLOAT,     .size = 4, .align = 4 };
Type *type_double      = &(Type){ .kind = TYPE_DOUBLE,    .size = 8, .align = 8 };
Type *type_bool        = &(Type){ .kind = TYPE_BOOL,      .size = 4, .align = 4 };


bool is_aggregate_type(Type *type) {
	return type->kind == TYPE_STRUCT || type->kind == TYPE_UNION;
}

bool is_floating_type(Type *type) {
	return type == type_float || type == type_double;
}

bool is_integer_type(Type *type) {
	return type == type_bool     ||
	       type == type_char     || type == type_schar     || type == type_uchar ||
	       type == type_short    || type == type_ushort    ||
	       type == type_int      || type == type_uint      || 
	       type == type_long     || type == type_ulong     || 
	       type == type_longlong || type == type_ulonglong ||
		   type->kind == TYPE_ENUM;
}

bool is_ptr_like_type(Type *type) {
	return type && (type->kind == TYPE_PTR || type->kind == TYPE_FUNC);
}

bool is_arithmetic_type(Type *type) {
	return is_integer_type(type) || is_floating_type(type);
}

bool is_scalar_type(Type *type) {
	return is_arithmetic_type(type) || type->kind == TYPE_PTR;
}

bool is_signed_integer_type(Type *type) {
	return type == type_schar    || 
	       type == type_short    || 
	       type == type_int      || 
	       type == type_long     || 
	       type == type_longlong;
}

bool is_unsigned_integer_type(Type *type) {
	return type == type_char      || 
	       type == type_uchar     || 
	       type == type_ushort    || 
	       type == type_uint      || 
	       type == type_ulong     || 
	       type == type_ulonglong;
}

void complete_type(Type *type);

Type *type_alloc(TypeKind kind) {
    Type *t = arena_alloc_zeroed(&type_arena, sizeof(Type));
    t->kind = kind;
    return t;
}

Type *type_ptr(Type *base) {
	Type *cached = map_get(&cached_ptr_types, base);
	if (cached) return cached;

    Type *t = type_alloc(TYPE_PTR);
    t->size = PTR_SIZE;
	t->align = PTR_SIZE;
    t->ptr.base = base;
	map_put(&cached_ptr_types, base, t);
    return t;
}

Type *type_array(Type *base, int num_items) {
    for (int i=0; i<da_len(cached_array_types); ++i) {
        Type *cached = cached_array_types[i];
        if (cached->array.base == base && cached->array.num_items == num_items)
            return cached;
    }

	complete_type(base);

    Type *t = type_alloc(TYPE_ARRAY);
    t->size = base->size * num_items;
	t->align = base->align;
    t->array.base = base;
    t->array.num_items = num_items;
    da_push(cached_array_types, t);
    return t;
}

Type *type_func(TypeField *params, int num_params, bool is_variadic, Type *ret) {
    for (int i=0; i<da_len(cached_func_types); ++i) {
        Type *cached = cached_func_types[i];
        if (cached->func.num_params == num_params && cached->func.ret == ret) {
            bool param_types_match = true;
            for (int j=0; j<cached->func.num_params; ++j) {
                if (params[j].type != cached->func.params[j].type) {
                    param_types_match = false;
                    break;
                }
            }
            if (param_types_match)
                return cached;
        }
    }

    Type *t = type_alloc(TYPE_FUNC);
    t->size = PTR_SIZE;
	t->align = PTR_SIZE;
    t->func.params = arena_memdup(&type_arena, params, num_params * sizeof(*params));
    t->func.num_params = num_params;
    t->func.is_variadic = is_variadic;
    t->func.ret = ret;
    da_push(cached_func_types, t);
    return t;
}

Type *type_enum(void) {
	Type *t = type_alloc(TYPE_ENUM);
	t->size = INT_SIZE;
	t->align = INT_SIZE;
	return t;
}

Type *type_incomplete(Sym *sym) {
    Type *t = type_alloc(TYPE_INCOMPLETE);
	t->sym = sym;
	return t;
}

void type_intern_test(void) {
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
    Type *func_one = type_func(params_one, ARRAY_COUNT(params_one), false, type_int);
    Type *func_two = type_func(params_two, ARRAY_COUNT(params_two), false, type_int);
    Type *func_three = type_func(NULL, 0, false, type_int);
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


