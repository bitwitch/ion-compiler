#define PTR_SIZE   8
#define INT_SIZE   4
#define FLOAT_SIZE 4
#define CHAR_SIZE  1

typedef struct Type Type;
typedef struct Sym Sym;

typedef enum {
    TYPE_NONE,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING,
	TYPE_VOID,
    TYPE_INT,
    TYPE_CHAR,
    TYPE_FLOAT,
	TYPE_BOOL,
    TYPE_PTR,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ENUM,
    TYPE_FUNC,
    TYPE_CONST,
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
            Type *ret;
        } func;
    };
};

Arena type_arena;
BUF(Type **cached_ptr_types);
BUF(Type **cached_array_types);
BUF(Type **cached_func_types);
Type *type_void   = &(Type){ .kind = TYPE_VOID };
Type *type_int    = &(Type){ .kind = TYPE_INT,   .size = INT_SIZE,   .align = INT_SIZE };
Type *type_char   = &(Type){ .kind = TYPE_CHAR,  .size = CHAR_SIZE,  .align = CHAR_SIZE };
Type *type_float  = &(Type){ .kind = TYPE_FLOAT, .size = FLOAT_SIZE, .align = FLOAT_SIZE };
Type *type_bool   = &(Type){ .kind = TYPE_BOOL,  .size = INT_SIZE,   .align = INT_SIZE };


void complete_type(Type *type);

Type *type_alloc(TypeKind kind) {
    Type *t = arena_alloc_zeroed(&type_arena, sizeof(Type));
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
    t->size = PTR_SIZE;
	t->align = PTR_SIZE;
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

	complete_type(base);

    Type *t = type_alloc(TYPE_ARRAY);
    t->size = base->size * num_items;
	t->align = base->align;
    t->array.base = base;
    t->array.num_items = num_items;
    da_push(cached_array_types, t);
    return t;
}

Type *type_func(TypeField *params, int num_params, Type *ret) {
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
    t->func.ret = ret;
    da_push(cached_func_types, t);
    return t;
}

Type *type_incomplete(Sym *sym) {
    Type *type = type_alloc(TYPE_INCOMPLETE);
	type->sym = sym;
	return type;
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


