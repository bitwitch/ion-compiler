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


