
/*
Grammar:


expr_base   = integer | '(' expr ')'
expr_pow    = expr_bas _(** expr_pow)*
expr_unary  = ([~-] expr_unary)* | expr_base
expr_mul    = expr_unary ([* % / >> << &] expr_unary)*
expr_add    = expr_mul ([+-^] expr_mul)*
expr        = expr_add



stmt = 'return' expr
     | 'for' for_stmt
     | 'do' do_stmt
     | 'while' while_stmt
     | ASSIGNMENT 



stmt_block = '{' stmt* '}'

    
enum_item = NAME ('=' expr)? ','?
enum_decl = '{' enum_item* '}'

field_item = NAME ':' type ','?
aggregate_decl = NAME '{' field_item+ '}'

var_decl = NAME ':' type ('=' expr)?
const_decl = NAME '=' expr

func_decl = NAME '(' func_parameter* ')' (':' type)? stmt_block

decl = 'enum' enum_decl
     | 'struct' aggregate_decl
     | 'union' aggregate_decl
     | 'var' var_decl
     | 'const' const_decl
     | 'func' func_decl
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
    int32_t val;
    Expr *left, *right;
};

typedef struct {
    char *name; 
    Expr *expr;
} EnumItem;

struct Typespec {

};

typedef struct {
    char *name;
    Typespec typespec;
} AggregateField;

typedef struct {
    AGGREGATE_NONE,
    AGGREGATE_STRUCT,
    AGGREGATE_UNION,
} AggregateKind;

typedef struct {
    DECL_NONE,
    DECL_ENUM,
    DECL_AGGREGATE,
    DECL_VAR,
    DECL_CONST,
    DECL_FUNC,
} DeclKind;

struct Decl {
    DeclKind kind;
    char *name;
    Expr *expr;
    union {
        EnumItem *enum_items;
        AggregateField *fields;
    };
};


Expr *parse_expr(void);
Expr *expr_unary(TokenKind op, Expr *expr);
Expr *expr_binary(TokenKind op, Expr *left, Expr *right);
Expr *expr_int(int32_t val);


