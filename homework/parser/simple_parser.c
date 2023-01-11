/*
 * Grammar for simple arithmetic expressions:
 *
 * expr3 = integer | '(' expr ')'
 * expr2 = ([~-] expr2)* | expr3
 * expr1 = expr2 ([* % / >> << &] expr2)*
 * expr0 = expr1 ([+-^] expr1)*
 * expr  = expr0
 */

#include <assert.h>
#include <stdio.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>

typedef enum {
    // reserving values 0 - 127 for tokens that are single ascii characters
	TOKEN_INT = 128,
} TokenKind;

typedef struct {
	TokenKind kind;
	int val;
} Token;

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


	return 0;
}
