/*
 *
 * Implement an expression parser for a simple arithmetic language:
 *
 * Parse an infix expression composed of integer literals and the following
 * operators, highest to lowest precedence:
 * 
 *     unary -, unary ~    (right associative)
 *     * / % << >> &       (left associative)
 *     + - | ^             (left associative)
 * 
 * Output an S-expression that corresponds to the parse tree, e.g.
 * 12 * (34 + 45) / 56 + ~25
 * 
 * should generate an S-expression that looks like this
 * (+ (+ (* 12 34) (/ 45 56)) (~ 25))
 *
 * Extra credit:
 * 
 * Q1: How would you support right associative binary operators like **
 *     (exponentiation)?
 * 
 * A1: Right associativity can be achieved in a particular grammar rule, by
 *     recurring on the right hand side of the binary operator. For example:
 *
 *     expr0 = expr1 (** expr0)*
 *
 *     This works because the expr0 on the right will effectively "consume" all
 *     expressions at this precedence level before the operator to its left is
 *     applied. You can think about this in the same way that the expr1 on the left 
 *     will "consume" all expressions of higher precedence before the current
 *     (lower) precedence rules will be matched.
 *
 * Q2: How would you support parenthesized expressions for explicit grouping?
 *
 * A2: At the highest level of precedence, you can add a matching rule for
 *     parenthesis and recur down to the lowest level of precedence to match
 *     whatever is inside the parenthesis. i.e. you add '(' expr ')'
 * 
 * While still using recursive descent, factor out the repetitive structure so
 * that the parsing for operators is driven by table information for what
 * operators exist and their precedence and associativity.
 * 
 * How might you use this to implement a language that supports user defined
 * operator symbols with user defined precedence and associativity?
 *
 * For more info see: 
 * https://github.com/pervognsen/bitwise/blob/master/notes/streams.md 
 */

#include <stdio.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#define STB_DS_IMPLEMENTATION
#include "../stb_ds.h"

typedef enum {
    // reserving values 0 - 127 for tokens that are single ascii characters
	TOKEN_INT = 128,
} TokenKind;

typedef struct {
	TokenKind kind;
	int val;
} Token;

/*typedef struct AST_Node AST_Node;*/
/*struct AST_Node {*/
    /*[>AST_NodeKind kind;<]*/
	/*AST_Node *left, *right;	*/
/*};*/

Token token = {0};
char *stream;

// Warning! this returns a pointer to a static buffer, so its contents will
// change on each call
char *str_token_kind(TokenKind kind) {
    static char str[64] = {0};
    if (kind == TOKEN_INT) {
        sprintf(str, "integer");
    } else {
        if (kind < 128 && isprint(kind))
            sprintf(str, "%c", kind);
        else 
            sprintf(str, "<ASCII %d>", kind);
    }
    return str;
}

void next_token(void) {
	while (isspace(*stream)) ++stream;

	if (isdigit(*stream)) {
		token.kind = TOKEN_INT;
		token.val = 0;
		while (isdigit(*stream)) {
			token.val *= 10;
			token.val += *stream - '0';
			++stream;
		}
	} 
	else {
		token.kind = *stream;
        ++stream;
	}
}


/*
 * Grammar for simple arithmetic expressions:
 * expr3 = integer | '(' expr ')'
 * expr2 = ([~-] expr2)* | expr3
 * expr1 = expr2 ([* % / >> << &] expr2)*
 * expr0 = expr1 ([+-^] expr1)*
 * expr  = expr0
 */


int parse_expr(void);

int parse_expr3(void) {
    if (token.kind == '(') {
        next_token();
        int val = parse_expr();
        if (token.kind == ')') {
            next_token();
            return val;
        } else {
            fprintf(stderr, "Expected ')', got %s\n", str_token_kind(token.kind));
            exit(1);
        }
    } else if (token.kind == TOKEN_INT) {
        int val = token.val;
        next_token();
        return val;
    } else {
        fprintf(stderr, "Expected integer or '(', got %s\n", str_token_kind(token.kind));
        exit(1);
    }
    return 0;
}

int parse_expr2(void) {
    if (token.kind == '~' || token.kind == '-') {
        char op = token.kind;
        next_token();
        if (op == '-')
            return -parse_expr2();
        else
            return ~parse_expr2();
    }
    return parse_expr3();
}

int parse_expr1(void) {
    int val = parse_expr2();
    while (token.kind == '*' || token.kind == '/' || token.kind == '%' ||
           token.kind == '&' || token.kind == '>' || token.kind == '<') // TODO(shaw): cheating here by just checking > instead of >>
    {
        char op = token.kind;
        next_token();
        int rval = parse_expr2();
        switch (op) {
            case '*': val  *= rval; break;
            case '/': assert(rval != 0); val /= rval; break;
            case '%': assert(rval != 0); val %= rval; break;
            case '&': val  &= rval; break;
            case '>': val >>= rval; break;
            case '<': val <<= rval; break;
            default:
                fprintf(stderr, "Expected *, /, %%, &, >>, or <<, got %s\n", str_token_kind(token.kind));
                exit(1);
                break;
        }
    }
    return val;
}

int parse_expr0(void) {
    int val = parse_expr1();
    while (token.kind == '+' || token.kind == '-' || token.kind == '^') {
        char op = token.kind;
        next_token();
        int rval = parse_expr1();
        switch (op) {
            case '+': val += rval; break;
            case '-': val -= rval; break;
            case '^': val ^= rval; break;
            default:
                fprintf(stderr, "Expected +, -, or ^, got %s\n", str_token_kind(token.kind));
                exit(1);
                break;
        }
    }
    return val;
}

int parse_expr(void) {
    return parse_expr0();
}


/*AST_Node *parse_expr0(Token *tokens) {*/

    /*parse_expr1();*/

    /*if (token.kind != '+' && token.kind != '-' && token.kind != '^') {*/
        /*fprintf(stderr, "Expected '+', '-', or '^', got %s\n", str_token_kind(kind));*/
        /*exit(1);*/
    /*}*/




	/*AST_Node *head = NULL;*/

	/*return head;*/
/*}*/

int parse_expression(char *source) {
    stream = source;
    next_token();
    return parse_expr();
}

int main(int argc, char **argv) {
	(void)argc; (void)argv;

#define TEST_PARSE_EXPRESSION(e) assert(parse_expression(#e) == (e))
    TEST_PARSE_EXPRESSION(3);
    TEST_PARSE_EXPRESSION(-3);
    TEST_PARSE_EXPRESSION(7+-4);
    TEST_PARSE_EXPRESSION((3));
    TEST_PARSE_EXPRESSION(60 + 9);
    TEST_PARSE_EXPRESSION(60 + 9 + 351);
    TEST_PARSE_EXPRESSION(300*2 + 33+33);
    TEST_PARSE_EXPRESSION(300*2 + 33+33);
    TEST_PARSE_EXPRESSION(300 * (2+33) + 33);

	/*Token *tokens = lex(expression);*/
	/*AST_Node *ast = parse_expr(tokens);*/


	return 0;
}
