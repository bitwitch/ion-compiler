/*
Grammar

enum_decl = NAME '{' (NAME ('=' expr)? ','?)* '}'
var_decl = NAME '=' expr
         | NAME ':' type ('=' expr)?
const_decl = NAME '=' expr
typedef_decl = NAME '=' type
directive_decl = NAME '(' (NAME '=' expr ','?)+ ')'
import_decl = NAME ('{' '...' | (NAME ',')+ '}')?

decl = 'enum' enum_decl
     | 'struct' aggregate_decl
     | 'union' aggregate_decl
     | 'var' var_decl
     | 'const' const_decl
     | 'typedef' typedef_decl
     | 'func' func_decl
	 | '#' directive_decl
	 | 'import' import_decl

for_init = 
assign_op = EQ | AUTO_EQ | ADD_EQ | SUB_EQ | MUL_EQ | DIV_EQ | MOD_EQ | LSHIFT_EQ | RSHIFT_EQ | XOR_EQ | AND_EQ | OR_EQ
stmt_block = '{' stmt* '}'
stmt = 'return' expr? ';'
     | 'continue' ';'
     | 'break' ';'
     | stmt_block
     | 'if' '(' expr ')' stmt_block elseif* ('else' stmt_block)?
     | 'for' '(' for_init? ';' expr? ';' for_next? ')' stmt_block
     | 'do' stmt_block 'while' '(' expr ')' ';'
     | 'while' '(' expr ')' stmt_block
     | 'switch' '(' expr ')' case* ('default' ':' stmt*)?
     | expr (INC | DEC | assign_op expr)?
     | 'defer' stmt ';'

base_type = NAME
          | 'func' '(' type_list? ')' (':' type)?
          | '(' type ')'
type = base_type ('[' expr? ']' | '*')*
typespec = NAME | '(' ':' type ')'

base_expr = INT
          | FLOAT
          | STR
		  | TRUE | FALSE
          | NAME
          | CAST '(' type ',' expr ')'
          | '(' expr ')'
          | typespec? '{' expr_list '}'
          | SIZEOF '(' ( ':' typespec | expr ) ')'
call_expr =  base_expr ('(' param* ')' | '[' expr ']' | '.' NAME)*
unary_expr = [+-&*~!] unary_expr
           | call_expr
mul_op = '*' | '/' | '%' | '&' | LSHIFT | RSHIFT
mul_expr = unary_expr (mul_op unary_expr)*
add_op = '+' | '-' | '|' | '^'
add_expr = mul_expr (add_op mul_expr)*
cmp_op = EQ_EQ | NOTEQ | LT | GT | LTEQ | GTEQ
cmp_expr = add_expr (cmp_op add_expr)*
and_expr = cmp_expr (AND cmp_expr)*
or_expr = and_expr (OR and_expr)*
ternary_expr = or_expr ('?' ternary_expr ':' ternary_expr)?
expr = ternary_expr
*/

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

struct Expr {
    ExprKind kind;
	SourcePos pos;
	struct Type *type;
	TokenMod mod;
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
            Expr **args;
            int num_args;
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
} Note;

typedef struct {
	SourcePos pos;
	Note *notes;
	int num_notes;
} NoteList;

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
	Expr *expr;
} DirectiveArg;

typedef struct {
	char *name;
	char *rename;
} ImportItem;

struct Decl {
    DeclKind kind;
	SourcePos pos;
    char *name;
	NoteList notes;  // annotations
    union {
        struct {
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
			DirectiveArg *args;
			int num_args;
		} directive;
		struct {
			bool is_relative;
			bool import_all;
			char *package_path;
			ImportItem *items; // items represents the imported symbols
			int num_items;
		} import;
    };
};


