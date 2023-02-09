// Lexing: translating char stream to token stream
// ---------------------------------------------------------------------------

typedef enum {
    // NOTE(shaw): reserving values 0-127 for ascii chars to use
	TOKEN_INT = 128,
	TOKEN_CHR,
	TOKEN_FLOAT,
	TOKEN_NAME,
	TOKEN_STR,
	TOKEN_KEYWORD,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
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


Token token;
char *stream;

char *keyword_enum;
char *keyword_struct;
char *keyword_union;
char *keyword_var;
char *keyword_const;
char *keyword_func;

void init_keywords(void) {
    static bool first = true;
    if (first) {
        keyword_enum = str_intern("enum");
        keyword_struct = str_intern("struct");
        keyword_union = str_intern("union");
        keyword_var = str_intern("var");
        keyword_const = str_intern("const");
        keyword_func = str_intern("func");
    }
    first = false;
}

bool is_keyword(char *check) {
    char *s = str_intern(check);
    return s == keyword_enum   ||
           s == keyword_struct ||
           s == keyword_union  ||
           s == keyword_var    ||
           s == keyword_const  ||
           s == keyword_func;
}

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
    token.str_val = str_intern_range(str_start, stream);
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

void next_token(void) {
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
            token.name = str_intern_range(token.start, stream);
            token.kind = is_keyword(token.name) ? TOKEN_KEYWORD : TOKEN_NAME;
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


// Warning! this returns a pointer to a static buffer, so its contents will
// change on each call
char *str_token_kind(TokenKind kind) {
    static char str[64] = {0};
    if (kind == TOKEN_INT) {
        sprintf(str, "integer");
    } else {
        if (kind < 128 && isprint(kind))
            sprintf(str, "%c", kind);
        else if (kind == TOKEN_LSHIFT)
            sprintf(str, "<<");
        else if (kind == TOKEN_RSHIFT)
            sprintf(str, ">>");
        else 
            sprintf(str, "<ASCII %d>", kind);
    }
    return str;
}

char *token_info(void) {
    if (token.kind == TOKEN_NAME || token.kind == TOKEN_KEYWORD)
        return token.name;
    return str_token_kind(token.kind);
}

bool is_token(TokenKind kind) {
    return token.kind == kind;
}

bool match_token(TokenKind kind) {
    if (token.kind == kind) {
        next_token();
        return true;
    } 
    else return false;
}

bool match_keyword(char *keyword) {
    if (token.kind == TOKEN_KEYWORD && token.name == keyword) {
        next_token();
        return true;
    }
    return false;
}

void expect_token(TokenKind kind) {
    if (token.kind == kind) {
        next_token();
    } else {
        syntax_error("Expected token '%s', got '%s'\n", 
                str_token_kind(kind), str_token_kind(token.kind));
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
#define assert_token_name(s) assert(token.name == str_intern(s) && match_token(TOKEN_NAME))
#define assert_token_keyword(s) assert(token.name == s && match_token(TOKEN_KEYWORD))
#define assert_token_int(i) assert(token.int_val == (i) && match_token(TOKEN_INT))
#define assert_token_float(f) assert(token.float_val == (f) && match_token(TOKEN_FLOAT))
#define assert_token_str(s) assert(token.str_val == str_intern(s) && match_token(TOKEN_STR))
#define assert_token_eof() assert(token.kind == 0)
void lex_test(void) {
    init_keywords();
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
        assert_token_keyword(keyword_const);
        assert_token_name("a");
        assert_token('=');
        assert_token_int(69);
        assert_token(';');
        assert_token_keyword(keyword_func);
        assert_token_name("pancake");
        assert_token('(');
        assert_token_name("i");
        assert_token(':');
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
#undef assert_token_str
#undef assert_token_eof


