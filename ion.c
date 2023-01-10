#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#define MAX(x, y) ((x) >= (y) ? (x) : (y))

// stretchy buffers, a la sean barrett

typedef struct {
	size_t len;
	size_t cap;
	char buf[1];
} DA_Header;

// get the metadata of the array which is stored before the actual buffer in
// memory
#define array__header(b) ((DA_Header*)((char*)b - offsetof(DA_Header, buf)))
// checks if n new elements will fit in the array
#define array__fits(b, n) (array_length(b) + (n) <= array_capacity(b)) 
// if n new elements will not fit in the array, grow the array by reallocating 
#define array__fit(b, n) (array__fits(b, n) ? 0 : ((b) = array__grow((b), array_length(b) + (n), sizeof(*(b)))))

#define array_length(b) ((b) ? array__header(b)->len : 0)
#define array_capacity(b) ((b) ? array__header(b)->cap : 0)
#define array_push(b, x) (array__fit(b, 1), (b)[array_length(b)] = (x), array__header(b)->len++)
#define array_free(b) ((b) ? (free(array__header(b)), (b) = NULL) : 0)

void *array__grow(void *buf, size_t new_len, size_t elem_size) {
	size_t new_cap = MAX(1 + 2*array_capacity(buf), new_len);
	assert(new_len <= new_cap);
	size_t new_size = offsetof(DA_Header, buf) + new_cap*elem_size;

	DA_Header *new_header;
	if (buf) {
		new_header = realloc(array__header(buf), new_size);
	} else {
		new_header = malloc(new_size);
		new_header->len = 0;
	}
	new_header->cap = new_cap;
	return new_header->buf;
}

// lexing: translating char stream to token stream

typedef enum {
	FIRST_NONCHAR_TOKEN = 128,
	TOKEN_INT,
	TOKEN_NAME,

} TokenKind;

typedef struct {
	TokenKind kind;
    char *start, *end;  // NOTE(shaw): this substring is not null terminated
	union {
		uint64_t u64;
	};
} Token;

Token token;
char *stream;

void next_token() {
    token.start = stream;
	switch (*stream) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
		{
			uint64_t val = 0;
			while (isdigit(*stream)) {
				val *= 10;
				val += *stream - '0';
				++stream;
			}
			token.kind = TOKEN_INT;
			token.u64 = val;
			break;
		}

		case 'a': case 'b': case 'c': case 'd': case 'e':
		case 'f': case 'g': case 'h': case 'i': case 'j':
		case 'k': case 'l': case 'm': case 'n': case 'o':
		case 'p': case 'q': case 'r': case 's': case 't':
		case 'u': case 'v': case 'w': case 'x': case 'y':
		case 'z': case 'A': case 'B': case 'C': case 'D':
		case 'E': case 'F': case 'G': case 'H': case 'I':
		case 'J': case 'K': case 'L': case 'M': case 'N':
		case 'O': case 'P': case 'Q': case 'R': case 'S':
		case 'T': case 'U': case 'V': case 'W': case 'X':
		case 'Y': case 'Z': case '_':
		{
			while (isalnum(*stream) || *stream == '_')
				++stream;
			token.kind = TOKEN_NAME;
			break;
		}
		default:
        {
			token.kind = *stream;
			++stream;
			break;
        }
	}
    token.end = stream;
}

void print_token(Token token) {
	switch(token.kind) {
	case TOKEN_INT:
		printf("TOKEN INT: %lu\n", token.u64);
		break;
	case TOKEN_NAME:
		printf("TOKEN NAME: %.*s\n", (int)(token.end - token.start), token.start);
		break;
	case FIRST_NONCHAR_TOKEN:
		break;
	default:
		printf("TOKEN '%c'\n", token.kind);
		break;
	}
	printf("\n");
}

void lex_test(void) {
	char *source = "+()_HELLO1,234+FOO!666";
	stream = source;
	next_token();
	while (token.kind) {
		print_token(token);
		next_token();
	}
}

void buf_test(void) {
	int *buf = NULL;

    assert(array_length(buf) == 0);

	enum { N = 1024 };
	for (int i=0; i < N; ++i) {
		array_push(buf, i);
	}
	assert(array_length(buf) == N);

	for (int i=0; i < N; ++i) {
		assert(buf[i] == i);
	}

	array_free(buf);
    assert(buf == NULL);
    assert(array_len(buf) == 0);
}

int main(int argc, char **argv) {
	buf_test();
	lex_test();



	return 0;
}
