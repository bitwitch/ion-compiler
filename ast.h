/*
Grammar

decl = 'enum' enum_decl
     | 'struct' aggregate_decl
     | 'union' aggregate_decl
     | 'var' var_decl
     | 'const' const_decl
     | 'typedef' typedef_decl
     | 'func' func_decl

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

base_type = NAME
          | 'func' '(' type_list? ')' (':' type)?
          | '(' type ')'
type = base_type ('[' expr? ']' | '*')*
typespec = NAME | '(' ':' type ')'

base_expr = INT
          | FLOAT
          | STR
          | NAME
          | CAST '(' expr ')'
          | '(' expr ')'
          | typespec? '{' expr_list '}'
call_expr =  base_expr ('(' param* ')' | '[' expr ']' | '.' NAME)*
unary_expr = [+-&*~] unary_expr
           | call_expr
mul_op = '*' | '/' | '%' | '&' | LSHIFT | RSHIFT
mul_expr = unary_expr (mul_op unary_expr)*
add_op = '+' | '-' | '|' | '^'
add_expr = mul_expr (add_op mul_expr)*
cmp_op = EQ | NOTEQ | LT | GT | LTEQ | GTEQ
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
    EXPR_FLOAT,
    EXPR_STR,
    EXPR_NAME,
    EXPR_TYPESPEC,
    EXPR_CAST,
    EXPR_CALL,
    EXPR_INDEX,
    EXPR_FIELD,
} ExprKind;

struct Expr {
    ExprKind kind;
    union {
        int32_t int_val;
        double float_val;
        char *str_val;
        char *name;
        struct {
            TokenKind op;
            Expr *expr;
        } unary;
        struct {
            TokenKind op;
            Expr *left, *right;
        } binary;
        struct {
            Expr *expr;
            Expr **args;
            int num_args;
        } call;
        struct {
            Typespec *type;
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
    };
};

typedef struct {
    char *name; 
    Expr *expr;
} EnumItem;

typedef struct {
    char *name;
    Typespec *type;
} AggregateField;

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
} StmtKind;

typedef enum {
    TYPESPEC_NONE,
    TYPESPEC_NAME,
    TYPESPEC_FUNC,
    TYPESPEC_ARRAY,
    TYPESPEC_POINTER,
} TypespecKind;

typedef struct {
    BUF(Typespec **arg_types);
    Typespec *ret_type;
} FuncTypespec;

struct Typespec {
    TypespecKind kind;
    union {
        char *name;
        struct {
            Typespec **args;
            size_t num_args;
            Typespec *ret;
        } func;
        struct {
            Typespec *elem;
            Expr *size;
        } array;
        struct {
            Typespec *elem;
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
    BUF(Expr **exprs);
    StmtBlock block;
} SwitchCase;


struct Stmt {
    StmtKind kind;
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
    };
};

typedef enum {
    DECL_NONE,
    DECL_ENUM,
    DECL_STRUCT,
    DECL_UNION,
    DECL_FUNC,
    DECL_VAR,
    DECL_CONST,
} DeclKind;

typedef struct {
    char *name;
    Typespec *type;
} FuncParam;

typedef struct {
    BUF(FuncParam *params);
    Typespec *ret_type;
} FuncDecl;

struct Decl {
    DeclKind kind;
    char *name;
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
            Typespec *ret_type;
            StmtBlock block;
        } func;
        struct {
            Typespec *type;
            Expr *expr;
        } var;
        struct {
            Expr *expr;
        } const_decl;
    };
};


