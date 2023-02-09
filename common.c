// Helper Utilities

#define array_count(a) sizeof(a)/sizeof(*(a))

void *xmalloc(size_t size) {
    void *ptr = malloc(size);
    if (ptr == NULL) {
        perror("malloc");
        exit(1);
    }
    return ptr;
}

void *xcalloc(size_t num_items, size_t item_size) {
    void *ptr = calloc(num_items, item_size);
    if (ptr == NULL) {
        perror("calloc");
        exit(1);
    }
    return ptr;
}

void *xrealloc(void *ptr, size_t size) {
    void *result = realloc(ptr, size);
    if (result == NULL) {
        perror("recalloc");
        exit(1);
    }
    return result;
}

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
// ---------------------------------------------------------------------------

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

#define BUF(x) x // annotates that x is a stretchy buffer
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
		new_header = xrealloc(da__header(buf), new_size);
	} else {
		new_header = xmalloc(new_size);
		new_header->len = 0;
	}
	new_header->cap = new_cap;
	return new_header->buf;
}

void da_test(void) {
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
// ---------------------------------------------------------------------------
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

    char *str = xmalloc(len + 1);
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


