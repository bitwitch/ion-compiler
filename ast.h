/*
Grammar

Declarations:

decl = 'enum' enum_decl
     | 'struct' aggregate_decl
     | 'union' aggregate_decl
     | 'var' var_decl
     | 'const' const_decl
     | 'typedef' typedef_decl
     | 'func' func_decl

Statements:

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



Expressions:

typespec = NAME | '(' ':' type ')'
base_expr = INT
          | FLOAT
          | STR
          | NAME
          | typespec? '{' expr_list '}'
          | CAST '(' expr ')'
          | '(' expr ')'
compound_expr =  base_expr ('(' param* ')' | '[' expr ']' | '.' NAME)*
unary_expr = [+-&*~] unary_expr
           | compound_expr
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
    EXPR_INT
} ExprKind;

struct Expr {
    ExprKind kind;
    TokenKind op;
    union {
        int32_t int_val;
        double float_val;
    };
    Expr *left, *right;
};

typedef struct {
    char *name; 
    Expr *expr;
} EnumItem;

typedef struct {
    char *name;
    Typespec *typespec;
} AggregateField;

typedef enum {
    AGGREGATE_NONE,
    AGGREGATE_STRUCT,
    AGGREGATE_UNION,
} AggregateKind;

typedef enum {
    DECL_NONE,
    DECL_ENUM,
    DECL_AGGREGATE,
    DECL_VAR,
    DECL_CONST,
    DECL_FUNC,
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
        BUF(EnumItem *enum_items); 
        BUF(AggregateField *aggregate_fields);
        FuncDecl func_decl;
        struct {
            Typespec *type;
            Expr *expr;
        };
    };
};


typedef enum {
    STMT_NONE,
    STMT_RETURN,
    STMT_CONTINUE,
    STMT_BREAK,
    STMT_BLOCK,
    STMT_IF,
    STMT_FOR,
    STMT_DO,
    STMT_WHILE,
    STMT_SWITCH,
    STMT_ASSIGN,
    STMT_AUTO_ASSIGN,
    STMT_EXPR,
} StmtKind;

typedef enum {
    TYPESPEC_NONE,
    TYPESPEC_PAREN,
    TYPESPEC_FUNC,
    TYPESPEC_NAME,
    TYPESPEC_ARRAY,
    TYPESPEC_POINTER,
} TypespecKind;

typedef struct {
    BUF(Typespec **arg_types);
    Typespec *ret_type;
} FuncTypespec;

struct Typespec {
    TypespecKind kind;
    struct {
        char *name;
        Expr *index;
        FuncTypespec func;
    };
};

typedef struct {
    BUF(Stmt *statements);
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
    Expr *expr;
    StmtBlock block;
    union {
        struct {
            char *var_name;
        };
        struct {
            ElseIf *else_ifs;
            StmtBlock else_block;
        };
        struct {
            BUF(SwitchCase *cases);
        };
        struct {
            StmtBlock for_init;
            StmtBlock for_next;
        };
        // Auto Assignment
        struct {
            char *name;
        };
        // Assignment operators
        struct {
            Expr *rhs;
        };
    };
};


