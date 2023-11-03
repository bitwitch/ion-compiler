typedef struct Decl Decl;
typedef struct Expr Expr;
typedef struct Typespec Typespec;
typedef struct Stmt Stmt;

typedef enum {
    EXPR_NONE,
    EXPR_UNARY,
    EXPR_BINARY,
    EXPR_TERNARY,
    EXPR_INT,
    EXPR_CHAR,
    EXPR_FLOAT,
	EXPR_BOOL,
    EXPR_STR,
    EXPR_NAME,
    EXPR_CAST,
    EXPR_CALL,
    EXPR_INDEX,
    EXPR_FIELD,
    EXPR_COMPOUND,
    EXPR_SIZEOF_EXPR,
    EXPR_SIZEOF_TYPE,
} ExprKind;

typedef struct {
	Expr *field_name;  // optional
	Expr *field_index; // optional
	Expr *field_value;
} CompoundArg;

struct Expr {
    ExprKind kind;
	SourcePos pos;
	struct Type *type;
	uint32_t mod; // modifiers, like an integer suffix ull
    union {
		uint64_t int_val;
        double float_val;
		bool bool_val;
        char *str_val;
		char char_val;
        char *name;
        Expr *sizeof_expr;
        Typespec *sizeof_typespec;
        struct {
            TokenKind op;
            Expr *expr;
        } unary;
        struct {
            TokenKind op;
            Expr *left, *right;
        } binary;
        struct {
            Expr *cond;
            Expr *then_expr;
            Expr *else_expr;
        } ternary;
        struct {
            Expr *expr;
            Expr **args;
            int num_args;
        } call;
        struct {
            Typespec *typespec;
            Expr *expr;
        } cast;
        struct {
            Expr *expr;
            Expr *index;
        } index;
        struct {
            Expr *expr;
            char *name;
        } field;
        struct {
            Typespec *typespec;
			CompoundArg *args;
            int num_args;
			bool is_designated_init;
        } compound;
    };
};

typedef struct {
    char *name;
    Typespec *typespec;
} AggregateField;

typedef enum {
    TYPESPEC_NONE,
    TYPESPEC_NAME,
    TYPESPEC_FUNC,
    TYPESPEC_ARRAY,
    TYPESPEC_POINTER,
} TypespecKind;

struct Typespec {
    TypespecKind kind;
	SourcePos pos;
    union {
        char *name;
        struct {
            Typespec **params;
            int num_params;
			bool is_variadic;
            Typespec *ret;
        } func;
        struct {
            Typespec *base;
            Expr *num_items;
        } array;
        struct {
            Typespec *base;
        } ptr;
    };
};


typedef struct {
    Stmt **stmts;
    int num_stmts;
} StmtBlock;

typedef struct {
    Expr *cond;
    StmtBlock block;
} ElseIf;

typedef struct {
    Expr **exprs;
    int num_exprs;
    bool is_default;
    StmtBlock block;
} SwitchCase;

typedef enum {
	STMT_NONE,
	STMT_RETURN,
	STMT_CONTINUE,
	STMT_BREAK,
	STMT_BRACE_BLOCK,
	STMT_IF,
	STMT_FOR,
	STMT_DO,
	STMT_WHILE,
	STMT_SWITCH,
	STMT_ASSIGN,
	STMT_INIT,
	STMT_EXPR,
	STMT_DEFER,
} StmtKind;

struct Stmt {
    StmtKind kind;
	SourcePos pos;
    union {
        Expr *expr;
        Decl *decl;
        StmtBlock block;
        struct {
            TokenKind op;
            Expr *left, *right;
        } assign;
        struct {
            char *name;
			Typespec *typespec;
            Expr *expr;
        } init;
        struct {
            Expr *cond;
            StmtBlock then_block;
            ElseIf *else_ifs;
            int num_else_ifs;
            StmtBlock else_block;
        } if_stmt;
        struct {
            Stmt *init;
            Expr *cond;
            Stmt *next;
            StmtBlock block;
        } for_stmt;
        struct {
            Expr *cond;
            StmtBlock block;
        } while_stmt;
        struct {
            Expr *expr;
            SwitchCase *cases;
            int num_cases;
        } switch_stmt;
        struct {
            Expr *expr;
        } return_stmt;
        struct {
			Stmt *stmt;
        } defer;
    };
};

// annotations
typedef struct {
	char *name;
	Expr *expr;
} NoteArg;

typedef struct {
	char *name;
	NoteArg *args;
	int num_args;
} Note;

typedef struct {
	SourcePos pos;
	Note *notes;
	int num_notes;
} NoteList;

typedef struct {
	char *name; 
	Expr *expr;
} EnumItem;

typedef struct {
    char *name;
    Typespec *typespec;
} FuncParam;

typedef struct {
	char *name;
	char *rename;
} ImportItem;

typedef enum {
	DIRECTIVE_NONE,
	DIRECTIVE_FOREIGN,
	DIRECTIVE_STATIC_ASSERT,
} DirectiveKind;

typedef struct {
	DirectiveKind kind;
	union {
		struct {
			NoteArg *args;
			int num_args;
		} foreign;
		struct {
			Expr *expr;
		} assert;
	};
} Directive;

typedef enum {
    DECL_NONE,
    DECL_ENUM,
    DECL_STRUCT,
    DECL_UNION,
    DECL_FUNC,
    DECL_VAR,
    DECL_CONST,
    DECL_TYPEDEF,
    DECL_DIRECTIVE,
    DECL_IMPORT,
} DeclKind;

struct Decl {
    DeclKind kind;
	SourcePos pos;
    char *name;
	NoteList notes;  // annotations
    union {
        struct {
			bool is_anonymous;
            EnumItem *items;
            int num_items;
        } enum_decl;
        struct {
            AggregateField *fields;
            int num_fields;
        } aggregate;
        struct {
            FuncParam *params;
            int num_params;
			bool is_variadic;
			bool is_incomplete; // true if function is declared but not defined
            Typespec *ret_typespec;
            StmtBlock block;
        } func;
        struct {
            Typespec *typespec;
            Expr *expr;
        } var;
        struct {
            Expr *expr;
        } const_decl;
        struct {
            Typespec *typespec;
        } typedef_decl;
		struct {
			bool is_relative;
			bool import_all;
			char *package_path;
			ImportItem *items; // items represents the imported symbols
			int num_items;
		} import;
		Directive directive;
    };
};


