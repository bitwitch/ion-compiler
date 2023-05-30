// Lexing: translating char stream to token stream
// ---------------------------------------------------------------------------

typedef enum {
    TOKEN_EOF,
	//
    // NOTE(shaw): reserving values 0-127 for ascii chars to use
	//
	TOKEN_INT = 128,
	TOKEN_CHAR,
	TOKEN_FLOAT,
	TOKEN_NAME,
	TOKEN_STR,
	TOKEN_KEYWORD,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_AUTO_ASSIGN,
    /*TOKEN_EQ,*/ // single literal ascii character token  for now
    TOKEN_ADD_EQ,
    TOKEN_SUB_EQ,
    TOKEN_MUL_EQ,
    TOKEN_DIV_EQ,
    TOKEN_MOD_EQ,
    TOKEN_AND_EQ,
    TOKEN_OR_EQ,
    TOKEN_XOR_EQ,
    TOKEN_LSHIFT_EQ,
    TOKEN_RSHIFT_EQ,
    TOKEN_INC,
    TOKEN_DEC,
    TOKEN_LOGICAL_OR,
    TOKEN_LOGICAL_AND,
    TOKEN_EQ_EQ,
    TOKEN_NOT_EQ,
    TOKEN_LT_EQ,
    TOKEN_GT_EQ,
	TOKEN_COMMENT,
	TOKEN_ELLIPSIS,
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
	SourcePos pos;
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
int current_line = 1;
char *compilation_filepath;

char *keyword_enum;
char *keyword_typedef;
char *keyword_struct;
char *keyword_union;
char *keyword_var;
char *keyword_const;
char *keyword_func;
char *keyword_return;
char *keyword_continue;
char *keyword_break;
char *keyword_if;
char *keyword_else;
char *keyword_for;
char *keyword_do;
char *keyword_while;
char *keyword_switch;
char *keyword_case;
char *keyword_default;
char *keyword_cast;
char *keyword_sizeof;

// non-keyword string interned names
char *name_foreign;

void init_keywords(void) {
    static bool first = true;
    if (first) {
        keyword_enum = str_intern("enum");
        keyword_typedef = str_intern("typedef");
        keyword_struct = str_intern("struct");
        keyword_union = str_intern("union");
        keyword_var = str_intern("var");
        keyword_const = str_intern("const");
        keyword_func = str_intern("func");
        keyword_return = str_intern("return");
        keyword_continue = str_intern("continue");
        keyword_break = str_intern("break");
        keyword_if = str_intern("if");
        keyword_else = str_intern("else");
        keyword_for = str_intern("for");
        keyword_do = str_intern("do");
        keyword_while = str_intern("while");
        keyword_switch = str_intern("switch");
        keyword_case = str_intern("case");
        keyword_default = str_intern("default");
        keyword_cast = str_intern("cast");
        keyword_sizeof = str_intern("sizeof");

		name_foreign = str_intern("foreign");
	}
    first = false;
}

bool is_keyword_name(char *check) {
    char *s = str_intern(check);
    return s == keyword_enum     ||
           s == keyword_typedef  ||
           s == keyword_struct   ||
           s == keyword_union    ||
           s == keyword_var      ||
           s == keyword_const    ||
           s == keyword_func     ||
           s == keyword_return   ||
           s == keyword_continue ||
           s == keyword_break    ||
           s == keyword_if       ||
           s == keyword_else     ||
           s == keyword_for      ||
           s == keyword_do       ||
           s == keyword_while    ||
           s == keyword_switch   ||
           s == keyword_case     ||
           s == keyword_default  ||
           s == keyword_cast     ||
           s == keyword_sizeof;
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

uint64_t scan_int_val(uint64_t base) {
    uint64_t val = 0;
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
	return val;
}

void scan_int(void) {
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

	uint64_t val = scan_int_val(base);

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

// simple escape sequence
bool check_escape_char(char c) {
	return c == 'n' || c == 't' || c == 'a'  || c == 'b' || c == 'f'  || 
	       c == 'r' || c == 'v' || c == '\\' || c == '\'' || c == '"' ||
		   c == '?';
}

void scan_escape_sequence(void) {
    ++stream; // skip opening backslash
	if (check_escape_char(*stream)) {
		++stream;
	} else if (*stream == 'x') {
		++stream;
		token.mod = TOKENMOD_HEX;
		uint64_t base = 16;
		token.int_val = scan_int_val(base);

	} else if (*stream == 'u' || *stream == 'U') {
		// TODO(shaw): universal character names
		assert(0);
	} else {
		syntax_error("Invalid escape sequence found in character literal, '\\%c'", *stream);
		++stream;
	}
}

void scan_str(void) {
    ++stream; // skip opening quote
    char *str_start = stream;
    while (*stream != '"') {
		if (*stream == '\\') {
			scan_escape_sequence();
		} else {
			if (!isprint(*stream)) {
				syntax_error("Invalid character found in string literal, '%c'", *stream);
			} 
			++stream;
		}
    }
    token.kind = TOKEN_STR;
    token.str_val = str_intern_range(str_start, stream);
    ++stream;
}

void scan_chr(void) {
    ++stream; // skip opening quote
    token.kind = TOKEN_CHAR;

	if (*stream == '\\') {
		scan_escape_sequence();
	} else {
		token.int_val = *stream;
		++stream;
	}
    if (*stream != '\'') {
        syntax_error("Expected closing single quote for character literal, but got '%c'", *stream);
    }
    ++stream;
}


// this is meant to match potential two character tokens
#define CASE2(c1, c2, k2) \
    case c1: \
        token.kind = c1; \
        stream++; \
        if (*stream == c2) { \
            token.kind = k2; \
            stream++; \
        } \
        break;


// this is meant to match potential two character tokens that have two possible
// combinations: c1-c2 or c1-c3
#define CASE2_ALT(c1, c2, k2, c3, k3) \
    case c1: \
        token.kind = c1; \
        ++stream; \
        if (*stream == c2) { \
            token.kind = k2; \
            ++stream; \
        } else if (*stream == c3) { \
            token.kind = k3; \
            ++stream; \
        } \
        break;


void next_token(void) {
repeat:
    token.start = stream;
	switch (*stream) {
        case ' ': case '\r': case '\t': case '\v': {
			while (isspace(*stream)) {
				if (*stream == '\n') break;
				++stream;
			}
            goto repeat;
            break;
        }

		case '\n': {
			++current_line;
			++stream;
			goto repeat;
			break;
		}

		case '/': {
			token.kind = '/';
			++stream;
			if (*stream == '=') {
				token.kind = TOKEN_DIV_EQ;
				++stream;
			} else if (*stream == '/') {
				while (*stream != '\n' && *stream != '\0')
					++stream;
				token.str_val = str_intern_range(token.start, stream);
				token.kind = TOKEN_COMMENT;	

				// @HACK TEMPORARY
				// just discarding comments for now, can think about if we want to 
				// store them so that they can be recovered for c codegen for example
				goto repeat;
			}
			break;
		}	

        case '\'': {
            scan_chr();
            break;
		}

        case '"': {
            scan_str();
            break;
		}

        case '.': {
            if (isdigit(stream[1])) {
                scan_float();
			} else if (stream[1] == '.' && stream[2] == '.') {
                token.kind = TOKEN_ELLIPSIS;
                stream += 3;
            } else {
                token.kind = *stream;
                ++stream;
            }
            break;
        }

        CASE2('=', '=', TOKEN_EQ_EQ)
        CASE2('!', '=', TOKEN_NOT_EQ)
        CASE2('*', '=', TOKEN_MUL_EQ)
        // CASE2('/', '=', TOKEN_DIV_EQ) // handled separately for comments
        CASE2('%', '=', TOKEN_MOD_EQ)
        CASE2('^', '=', TOKEN_XOR_EQ)
        CASE2(':', '=', TOKEN_AUTO_ASSIGN)

        CASE2_ALT('+', '+', TOKEN_INC,         '=', TOKEN_ADD_EQ)
        CASE2_ALT('-', '-', TOKEN_DEC,         '=', TOKEN_SUB_EQ)
        CASE2_ALT('&', '&', TOKEN_LOGICAL_AND, '=', TOKEN_AND_EQ)
        CASE2_ALT('|', '|', TOKEN_LOGICAL_OR,  '=', TOKEN_OR_EQ)

        case '>': {
			token.kind = *stream;
			++stream;
            if (*stream == '>') {
                token.kind = TOKEN_RSHIFT;
                ++stream;
                if (*stream == '=') {
                    token.kind = TOKEN_RSHIFT_EQ;
                    ++stream;
                }
            } else if (*stream == '=') {
                token.kind = TOKEN_GT_EQ;
                ++stream;
            }
            break;
        }

        case '<': {
			token.kind = *stream;
			++stream;
            if (*stream == '<') {
                token.kind = TOKEN_LSHIFT;
                ++stream;
                if (*stream == '=') {
                    token.kind = TOKEN_LSHIFT_EQ;
                    ++stream;
                }
            } else if (*stream == '=') {
                token.kind = TOKEN_LT_EQ;
                ++stream;
            }
            break;
        }

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9': {
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
		case 'Y': case 'Z': case '_': {
            while (isalnum(*stream) || *stream == '_')
                ++stream;
            token.name = str_intern_range(token.start, stream);
            token.kind = is_keyword_name(token.name) ? TOKEN_KEYWORD : TOKEN_NAME;
			break;
		}

		default: {
			token.kind = *stream;
			++stream;
			break;
        }
	}
	token.pos.filepath = compilation_filepath;
	token.pos.line = current_line;
    token.end = stream;
}

void init_stream(char *path, char *source) {
    stream = source;
	compilation_filepath = path;
	current_line = 1;
    next_token();
}

// Warning! this returns a pointer to a static buffer, so its contents will
// change on each call
char *token_kind_to_str(TokenKind kind) {
    static char str[64] = {0};
    switch(kind) {
    case TOKEN_INT:         sprintf(str, "integer"); break;
    case TOKEN_FLOAT:       sprintf(str, "float");   break;
    case TOKEN_AUTO_ASSIGN: sprintf(str, ":=");      break;
    case TOKEN_NAME:        sprintf(str, "name");    break;
    case TOKEN_EQ_EQ:       sprintf(str, "==");      break;
    case TOKEN_NOT_EQ:      sprintf(str, "!=");      break;
    case TOKEN_LT_EQ:       sprintf(str, "<=");      break;
    case TOKEN_GT_EQ:       sprintf(str, ">=");      break;
    case TOKEN_LSHIFT_EQ:   sprintf(str, "<<=");     break;
    case TOKEN_RSHIFT_EQ:   sprintf(str, ">>=");     break;
    case TOKEN_LSHIFT:      sprintf(str, "<<");      break;
    case TOKEN_RSHIFT:      sprintf(str, ">>");      break;
    case TOKEN_INC:         sprintf(str, "++");      break;
    case TOKEN_DEC:         sprintf(str, "--");      break;
    case TOKEN_ADD_EQ:      sprintf(str, "+=");      break;
    case TOKEN_SUB_EQ:      sprintf(str, "-=");      break;
    case TOKEN_MUL_EQ:      sprintf(str, "*=");      break;
    case TOKEN_DIV_EQ:      sprintf(str, "/=");      break;
    case TOKEN_MOD_EQ:      sprintf(str, "%%=");     break;
    case TOKEN_AND_EQ:      sprintf(str, "&=");      break;
    case TOKEN_OR_EQ:       sprintf(str, "|=");      break;
    case TOKEN_XOR_EQ:      sprintf(str, "^=");      break;
    case TOKEN_LOGICAL_OR:  sprintf(str, "||");      break;
    case TOKEN_LOGICAL_AND: sprintf(str, "&&");      break;
    default:
        if (kind < 128 && isprint(kind)) {
            sprintf(str, "%c", kind);
        } else {
            sprintf(str, "<ASCII %d>", kind);
        }
        break;
    }

    return str;
}

char *token_info(void) {
    if (token.kind == TOKEN_NAME || token.kind == TOKEN_KEYWORD)
        return token.name;
    return token_kind_to_str(token.kind);
}

bool is_token(TokenKind kind) {
    return token.kind == kind;
}

bool is_token_cmp(void) {
    return token.kind == TOKEN_EQ_EQ  ||
           token.kind == TOKEN_NOT_EQ ||
           token.kind == TOKEN_LT_EQ  ||
           token.kind == TOKEN_GT_EQ  ||
           token.kind == '<'          ||
           token.kind == '>';
}


bool match_token(TokenKind kind) {
    if (token.kind == kind) {
        next_token();
        return true;
    } 
    else return false;
}

bool is_keyword(char *keyword) {
    return token.kind == TOKEN_KEYWORD && token.name == keyword;
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
        // FIXME(shaw): token_kind_to_str needs to be fixed to not return a
        // pointer to a static buffer, arenas will probably be good for that
        char *tmp = token_kind_to_str(kind);
        char *expected = strdup(tmp);
        syntax_error("Expected token '%s', got '%s'\n", 
                expected, token_kind_to_str(token.kind));
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
        char *source = "func variadic (n: int, ...);";
		init_stream("", source);
		assert_token_keyword(keyword_func);
        assert_token_name("variadic");
        assert_token('(');
        assert_token_name("n");
        assert_token(':');
        assert_token_name("int");
        assert_token(',');
        assert_token(TOKEN_ELLIPSIS);
	}
    {
        char *source = "+()_HELLO1,234+FOO!666";
        init_stream("", source);
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
        init_stream("", source);
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
        init_stream("", source);
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
		init_stream("", source);
        assert_token_float(3.14159);
        assert_token_float(0.12);
        assert_token_float(7.0);
        assert_token_float(5.0);
        assert_token_float(0.33);
        assert_token_eof();
    }
    {
        char *source = "\"sally\" \"Geronimo\" fart poop 69";
		init_stream("", source);
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


