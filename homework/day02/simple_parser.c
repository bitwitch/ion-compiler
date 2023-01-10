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
 * 
 * 12*34 + 45/56 + ~25
 * 
 * should generate an S-expression that looks like this
 * 
 * (+ (+ (* 12 34) (/ 45 56)) (~ 25))
 *
 * Extra credit:
 * 
 * How would you support right associative binary operators like **
 * (exponentiation)?
 * 
 * How would you support parenthesized expressions for explicit grouping?
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
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#define STB_DS_IMPLEMENTATION
#include "../stb_ds.h"

typedef enum {
	FIRST_NONASCII_TOKEN = 128,
	TOKEN_INT,
} Token_Kind;

typedef struct {
	Token_Kind kind;
	int val;
} Token;

typedef struct AST_Node AST_Node;
struct AST_Node {
	AST_Node *left, *right;	
};

Token token = {0};
char *stream;

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
	}
}

Token *lex(char *expression) {
	Token *tokens = NULL;
	stream = expression;
	do {
		next_token();
		arrput(tokens, token);
	} while (token.kind);
	return tokens;
}

AST_Node *parse(Token *tokens) {
	(void)tokens;
	AST_Node *head = NULL;

	return head;
}

int main(int argc, char **argv) {
	(void)argc; (void)argv;

	char *expression = "12*34 + 45/56 + ~25";

	Token *tokens = lex(expression);
	AST_Node *ast = parse(tokens);
	(void)ast;

	return 0;
}
