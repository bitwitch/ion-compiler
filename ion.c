#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>


// dynamic array or "stretchy buffers", a la sean barrett

typedef struct {
	size_t len;
	size_t cap;
	char buf[]; // flexible array member
} DA_Header;

#define MAX(x, y) ((x) >= (y) ? (x) : (y))

// get the metadata of the array which is stored before the actual buffer in memory
#define da__header(b) ((DA_Header*)((char*)b - offsetof(DA_Header, buf)))
// checks if n new elements will fit in the array
#define da__fits(b, n) (da_lenu(b) + (n) <= da_cap(b)) 
// if n new elements will not fit in the array, grow the array by reallocating 
#define da__fit(b, n) (da__fits(b, n) ? 0 : ((b) = da__grow((b), da_lenu(b) + (n), sizeof(*(b)))))

#define da_len(b)  ((b) ? (int32_t)da__header(b)->len : 0)
#define da_lenu(b) ((b) ?          da__header(b)->len : 0)
#define da_cap(b) ((b) ? da__header(b)->cap : 0)
#define da_end(b) ((b) + da_lenu(b))
#define da_push(b, ...) (da__fit(b, 1), (b)[da__header(b)->len++] = (__VA_ARGS__))
#define da_free(b) ((b) ? (free(da__header(b)), (b) = NULL) : 0)

void *da__grow(void *buf, size_t new_len, size_t elem_size) {
	size_t new_cap = MAX(1 + 2*da_cap(buf), new_len);
	assert(new_len <= new_cap);
	size_t new_size = offsetof(DA_Header, buf) + new_cap*elem_size;

	DA_Header *new_header;
	if (buf) {
		new_header = realloc(da__header(buf), new_size);
	} else {
		new_header = malloc(new_size);
		new_header->len = 0;
	}
	new_header->cap = new_cap;
	return new_header->buf;
}

// String Interning
typedef struct {
    size_t len;
    char *str;
} InternStr;

static InternStr *interns;

char *str_intern_range(char *start, char *end) {
    size_t len = end - start;

    // check if string is already interned
    for (size_t i=0; i<da_lenu(interns); ++i) {
        if (len == interns[i].len && strncmp(interns[i].str, start, len) == 0) {
            return interns[i].str;
        }
    }

    char *str = malloc(len + 1);
    strncpy(str, start, len);
    str[len] = 0;
    /*InternStr intern_str = { len, str };*/

    da_push(interns, (InternStr){ len, str });

    return str;
}

char *str_intern(char *str) {
    // FIXME(shaw): just wrapping the range version for now with a wasteful
    // call to strlen. we can be smarter about this but its fine for now.
    return str_intern_range(str, str + strlen(str));
}

void str_intern_test(void) {
    char a[] = "boobies";
    char b[] = "boobies";
    char c[] = "boobies!";

    char *start = a; 
    char *end = a + strlen(a);

    assert(a != b);
    assert(str_intern(a) == str_intern(b));
    assert(str_intern(a) != str_intern(c));
    assert(str_intern(a) == str_intern_range(start, end));
}


// lexing: translating char stream to token stream

typedef enum {
    // NOTE(shaw): reserving values 0-127 for ascii chars to use
	TOKEN_INT = 128,
	TOKEN_NAME,
} TokenKind;

typedef struct {
	TokenKind kind;
    char *start, *end;
	union {
		uint64_t int_val;
        char *name;
	}; // anonymous unions are c11 feature
} Token;

Token token;
char *stream;

uint64_t scan_int(void) {
    uint64_t val = 0;
    while (isdigit(*stream)) {
        val *= 10;
        val += *stream - '0';
        ++stream;
    }
    return val;
}

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
            token.kind = TOKEN_INT;
            token.int_val = scan_int();
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
            token.name = str_intern_range(token.start, stream);
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

void init_stream(char *source) {
    stream = source;
    next_token();
}

bool match_token(TokenKind kind) {
    if (token.kind == kind) {
        next_token();
        return true;
    } 
    else return false;
}

void print_token(Token token) {
	switch(token.kind) {
	case TOKEN_INT:
		printf("TOKEN INT: %llu\n", token.int_val);
		break;
	case TOKEN_NAME:
		printf("TOKEN NAME: %.*s\n", (int)(token.end - token.start), token.start);
		break;
	default:
		printf("TOKEN '%c'\n", token.kind);
		break;
	}
	printf("\n");
}

#define assert_token(k) assert(match_token(k))
#define assert_token_name(s) assert(str_intern(token.name) == str_intern(s) && match_token(TOKEN_NAME))
#define assert_token_int(i) assert(token.int_val == (i) && match_token(TOKEN_INT))
#define assert_token_eof() assert(token.kind == 0)
void lex_test(void) {
	char *source = "+()_HELLO1,234+FOO!666";
    init_stream(source);
    assert_token('+');
    assert_token('(');
    assert_token(')');
    assert_token_name("_HELLO1");
    assert_token(',');
    assert_token_int(234);
    assert_token('+');
    assert_token_name("FOO");
    assert_token('!');
    assert_token_int(666);
    assert_token_eof();
}
#undef assert_token
#undef assert_token_name
#undef assert_token_int
#undef assert_token_eof

void buf_test(void) {
	int *buf = NULL;

    assert(da_len(buf) == 0);

    int n = 1024;
	for (int i=0; i < n; ++i) {
		da_push(buf, i);
	}
	assert(da_len(buf) == n);

	for (int i=0; i < n; ++i) {
		assert(buf[i] == i);
	}

	da_free(buf);
    assert(buf == NULL);
    assert(da_len(buf) == 0);
}

int main(int argc, char **argv) {
    (void)argc; (void)argv;
	buf_test();
	lex_test();
    str_intern_test();

	return 0;
}
