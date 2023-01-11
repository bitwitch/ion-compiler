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
#include <stdlib.h>
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

typedef struct AST_Node AST_Node;
struct AST_Node {
    Token token;
    AST_Node *left, *right;
};

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

bool is_token(TokenKind kind) {
    return token.kind == kind;
}

bool match_token(TokenKind kind) {
    if (token.kind == kind) {
        next_token();
        return true;
    }
    return false;
}


AST_Node *parse_expr(void);

AST_Node *parse_expr3(void) {
    AST_Node *current = NULL;
    if (is_token(TOKEN_INT)) {
        current = malloc(sizeof(AST_Node));
        current->token = token;
        next_token();
        current->left = NULL;
        current->right = NULL;
    } else if (match_token('(')) {
        current = parse_expr();
        if (!match_token(')')) {
            fprintf(stderr, "Expected ')', got %s\n", str_token_kind(token.kind));
            exit(1);
        }
    } else {
        fprintf(stderr, "Expected integer or '(', got %s\n", str_token_kind(token.kind));
        exit(1);
    }

    return current;
}

AST_Node *parse_expr2(void) {
    if (is_token('-') || is_token('~')) {
        AST_Node *current = malloc(sizeof(AST_Node));
        current->token = token;
        current->left = NULL;
        next_token();
        current->right = parse_expr3();
        return current;
    }
    return parse_expr3();
}

AST_Node *parse_expr1(void) {
    AST_Node *left = parse_expr2();
    AST_Node *current = left;

    while (is_token('*') || is_token('/') || is_token('%') || 
           is_token('&') || is_token('<') || is_token('>')) // TODO(shaw): cheating here by just checking > instead of >>
    {
        current = malloc(sizeof(AST_Node));
        current->token = token;
        current->left = left;
        next_token();
        current->right = parse_expr2();
        left = current;
    }

    return current;
}

/*
200*3 - 55 - 66

                   -
          -              66
    *        55
200  3
*/
AST_Node *parse_expr0(void) {
    AST_Node *left = parse_expr1();
    AST_Node *current = left;

    while (is_token('+') || is_token('-') || is_token('^')) {
        current = malloc(sizeof(AST_Node));
        current->token = token;
        current->left = left;
        next_token();
        current->right = parse_expr1();
        left = current;
    }

    return current;
}

AST_Node *parse_expr(void) {
    return parse_expr0();
}

AST_Node *parse_expression(char *source) {
    stream = source;
    next_token();
    return parse_expr();
}

void print_s_expression(AST_Node *head) {
    if (head == NULL) return;

    if (head->token.kind == TOKEN_INT) 
        printf("%d", head->token.val);
    else if (head->token.kind == 0) {

    } 
    else
        printf("(%c ", head->token.kind);

    if (head->left) {
        print_s_expression(head->left);
        printf(" ");
    }
    if (head->right) {
        print_s_expression(head->right);
        printf(")");
    }
}

/*
#define TEST_PARSE_EXPRESSION(e) assert(parse_expression(#e) == (e))
void parse_test(void) {
    TEST_PARSE_EXPRESSION(3);
    TEST_PARSE_EXPRESSION(-3);
    TEST_PARSE_EXPRESSION(7+-4);
    TEST_PARSE_EXPRESSION((3));
    TEST_PARSE_EXPRESSION(60 + 9);
    TEST_PARSE_EXPRESSION(60 + 9 + 351);
    TEST_PARSE_EXPRESSION(300*2 + 33+33);
    TEST_PARSE_EXPRESSION(300*2 + 33+33);
    TEST_PARSE_EXPRESSION(300 * (2+33) + 33);
}
#undef TEST_PARSE_EXPRESSION
*/

int main(int argc, char **argv) {
	(void)argc; (void)argv;
    /*parse_test();*/

    AST_Node *head = parse_expression("12*34 + 45/56 + ~25");
    print_s_expression(head);
    printf("\n");

	return 0;
}





