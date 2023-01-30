#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <math.h>


// Helper Utilities
#define array_count(a) sizeof(a)/sizeof(*(a))

void fatal(char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("FATAL: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
    exit(1);
}

void syntax_error(char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("Syntax Error: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
}

// dynamic array or "stretchy buffers", a la sean barrett

typedef struct {
	size_t len;
	size_t cap;
	char buf[]; // flexible array member
} DA_Header;

#define MAX(x, y) ((x) >= (y) ? (x) : (y))
#define BUF(x) x // annotates that x is a stretchy buffer

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
	TOKEN_CHR,
	TOKEN_FLOAT,
	TOKEN_STR,
	TOKEN_NAME,
} TokenKind;

typedef enum {
	TOKENMOD_NONE,
	TOKENMOD_HEX,
	TOKENMOD_BIN,
	TOKENMOD_OCT,
} TokenMod;

typedef struct {
	TokenKind kind;
	TokenMod mod;
    char *start, *end;
	union {
		uint64_t int_val;
        double float_val;
        char *str_val;
        char *name;
	};
} Token;

bool match_token(TokenKind kind);
void expect_token(TokenKind kind);


Token token;
char *stream;

uint8_t char_to_digit[256] = {
    ['0'] = 0,
    ['1'] = 1,
    ['2'] = 2,
    ['3'] = 3,
    ['4'] = 4,
    ['5'] = 5,
    ['6'] = 6,
    ['7'] = 7,
    ['8'] = 8,
    ['9'] = 9,
    ['a'] = 10, ['A'] = 10,
    ['b'] = 11, ['B'] = 11,
    ['c'] = 12, ['C'] = 12,
    ['d'] = 13, ['D'] = 13,
    ['e'] = 14, ['E'] = 14,
    ['f'] = 15, ['F'] = 15,
};

void scan_int(void) {
    uint64_t val = 0;
    uint64_t base = 10;

    if (*stream == '0') {
        ++stream;
        if (tolower(*stream) == 'x') {
            ++stream;
            token.mod = TOKENMOD_HEX;
            base = 16;
        } else if (tolower(*stream) == 'b') {
            ++stream;
            token.mod = TOKENMOD_BIN;
            base = 2;
        } else if (isdigit(*stream)) {
            token.mod = TOKENMOD_OCT;
            base = 8;
        }
    }

    for (;;) {
        uint64_t digit = char_to_digit[(int)*stream];
        if (digit == 0 && *stream != '0') {
            break;
        }
        if (digit >= base) {
            syntax_error("Digit '%c' out of range for base %llu", *stream, base);
            digit = 0;
        }
        if (val > (UINT64_MAX - digit)/base) {
            syntax_error("Integer literal overflow");
            while (isdigit(*stream)) 
                ++stream;
            val = 0;
            break;
        }
        val = val * base + digit;
        ++stream;
    }

    token.kind = TOKEN_INT;
    token.int_val = val;
}

void scan_float(void) {
    char *start = stream;
    while (isdigit(*stream))
        ++stream;
    if (*stream == '.')
        ++stream;
    while (isdigit(*stream))
        ++stream;
    if (tolower(*stream) == 'e') {
        ++stream;
        if (*stream == '+' || *stream == '-')
            ++stream;
        if (!isdigit(*stream))
            syntax_error("Expected digit after float literal exponent, found '%c'.", *stream);
        while (isdigit(*stream))
            ++stream;
    }
    double val = strtod(start, NULL);
    if (val == HUGE_VAL || val == -HUGE_VAL)
        syntax_error("Float literal overflow");
    token.kind = TOKEN_FLOAT;
    token.float_val = val;
}

void scan_str(void) {
    ++stream; // skip opening quote
    char *str_start = stream;
    while (*stream != '"') {
        if (!isprint(*stream)) {
            syntax_error("Invalid character found in string literal, '%c'", *stream);
        }
        ++stream;
    }
    token.kind = TOKEN_STR;
    token.str_val = str_intern_range(str_start, stream-1);
    ++stream;
}

void scan_chr(void) {
    ++stream; // skip opening quote
    token.kind = TOKEN_CHR;
    token.int_val = *stream;
    if (*stream != '\'') {
        syntax_error("Expected closing single quote for character literal, but got '%c'", *stream);
    }
    ++stream;
}




void next_token() {
repeat:
    token.start = stream;
	switch (*stream) {
        case ' ': case '\n': case '\r': case '\t': case '\v':
        {
            while (isspace(*stream)) ++stream;
            goto repeat;
            break;
        }

        /*case '\'':*/
            /*scan_chr();*/
            /*break;*/
        case '"':
            scan_str();
            break;
        case '.':
            scan_float();
            break;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		{
            while (isdigit(*stream)) 
                ++stream;
            char c = *stream;
            stream = token.start;
            if (c == '.' || tolower(c) == 'e')
                scan_float();
            else
                scan_int();
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
    return false;
}

char *str_token_kind(TokenKind kind) {
    (void)kind;
    return "str_token_kind_stub";
}

void expect_token(TokenKind kind) {
    if (token.kind == kind) {
        next_token();
    } else {
        syntax_error("Expected token '%s', got '%s'", str_token_kind(kind), str_token_kind(token.kind));
        exit(1);
    }
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
#define assert_token_float(f) assert(token.float_val == (f) && match_token(TOKEN_FLOAT))
#define assert_token_str(s) assert(token.str_val == str_intern(s) && match_token(TOKEN_STR))
#define assert_token_eof() assert(token.kind == 0)
void lex_test(void) {
    {
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
    {
        char *source = "239 0xc0ffee 0Xabc123 0b11011110 01234 0";
        init_stream(source);
        assert_token_int(239);
        assert_token_int(0xc0ffee);
        assert_token_int(0xabc123);
        assert_token_int(0xDE);
        assert_token_int(01234);
        assert_token_int(0);
        assert_token_eof();
    }
    {
        char *source = "const a = 69;\nfunc pancake(i: int) { bake(); }";
        init_stream(source);
        /*assert_token_keyword(keyword_const);*/
        assert_token_name("const");
        assert_token_name("a");
        assert_token('=');
        assert_token_int(69);
        assert_token(';');
        /*assert_token_keyword(keyword_func);*/
        assert_token_name("func");
        assert_token_name("pancake");
        assert_token('(');
        assert_token_name("i");
        assert_token(':');
        /*assert_token_keyword(keyword_int);*/
        assert_token_name("int");
        assert_token(')');
        assert_token('{');
        assert_token_name("bake");
        assert_token('(');
        assert_token(')');
        assert_token(';');
        assert_token('}');
        assert_token_eof();
    }
    {
        char *source = "3.14159 0.12 7. 5.0 .33";
        init_stream(source);
        assert_token_float(3.14159);
        assert_token_float(0.12);
        assert_token_float(7.0);
        assert_token_float(5.0);
        assert_token_float(0.33);
        assert_token_eof();
    }
    {
        char *source = "\"sally\" \"Geronimo\" fart poop 69";
        init_stream(source);
        assert_token_str("sally");
        assert_token_str("Geronimo");
        assert_token_name("fart");
        assert_token_name("poop");
        assert_token_int(69);
        assert_token_eof();
    }
}
#undef assert_token
#undef assert_token_name
#undef assert_token_int
#undef assert_token_float
#undef assert_token_eof



int main(int argc, char **argv) {
    (void)argc; (void)argv;
	buf_test();
    str_intern_test();
	lex_test();

	return 0;
}
