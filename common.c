#define ARRAY_COUNT(a) sizeof(a)/sizeof(*(a))
#define MAX(x, y) ((x) >= (y) ? (x) : (y))
#define IS_POW2(x) (((x) != 0) && ((x) & ((x)-1)) == 0)

// Helper Utilities
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

// allocating sprintf
char *strf(char *fmt, ...) {
    // get length
    va_list args;
    va_start(args, fmt);
    int size = 1 + vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    // allocate and write to buffer
    char *str = xmalloc(size);
    va_start(args, fmt);
    vsnprintf(str, size, fmt, args);
    va_end(args);
    return str;
}

void strf_test(void) {
    char *str = strf("eat my %s", "burger");
    str = strf("bob says %s", str);
    
    printf("%s\n", str);
}

// File IO

// this is the size of a chunk of data in each read. one is added to this in
// the actual call to fread to leave space for a null character
#ifndef  READFILE_CHUNK
#define READFILE_CHUNK 2097152 // 2MiB
#endif

#define  READ_ENTIRE_FILE_OK          0  /* Success */
#define  READ_ENTIRE_FILE_INVALID    -1  /* Invalid parameters */
#define  READ_ENTIRE_FILE_ERROR      -2  /* Stream error */
#define  READ_ENTIRE_FILE_TOOMUCH    -3  /* Too much input */
#define  READ_ENTIRE_FILE_NOMEM      -4  /* Out of memory */

int read_entire_file(FILE *fp, char **dataptr, size_t *sizeptr) {
	/*
	* See answer by Nominal Animal (note this is not the accepted answer)
	* https://stackoverflow.com/questions/14002954/c-programming-how-to-read-the-whole-file-contents-into-a-buffer#answer-44894946
	*/
	char *data = NULL, *temp;
	uint64_t bytes_allocated = 0;
	uint64_t read_so_far = 0;
	uint64_t n; // bytes read in a single fread call

	/* None of the parameters can be NULL. */
	if (fp == NULL || dataptr == NULL || sizeptr == NULL)
		return READ_ENTIRE_FILE_INVALID;

	/* A read error already occurred? */
	if (ferror(fp))
		return READ_ENTIRE_FILE_ERROR;

	while (1) {
		/* first check if buffer is large enough to read another chunk */
		uint64_t new_size = read_so_far + READFILE_CHUNK + 1;

		if (bytes_allocated < new_size) {
			/* need to grow the buffer */
			bytes_allocated = new_size;

			/* overflow check */
			if (new_size <= read_so_far) {
				free(data);
				return READ_ENTIRE_FILE_TOOMUCH;
			}

			temp = realloc(data, new_size);
			if (!temp) {
				free(data);
				return READ_ENTIRE_FILE_NOMEM;
			}
			data = temp;
		}

		/* read in a chunk */
		n = fread(data+read_so_far, sizeof(char), READFILE_CHUNK, fp);
		if (n == 0)
			break;

		read_so_far += n;
	}

	if (ferror(fp)) {
		free(data);
		return READ_ENTIRE_FILE_ERROR;
	}

	/* resize the buffer to the exact length of the file (plus 1 for null termination) */
	temp = realloc(data, read_so_far + 1);
	if (!temp) {
		free(data);
		return READ_ENTIRE_FILE_NOMEM;
	}
	data = temp;
	data[read_so_far] = '\0';

	*dataptr = data;
	*sizeptr = read_so_far;
	return READ_ENTIRE_FILE_OK;
}


// dynamic array or "stretchy buffers", a la sean barrett
// ---------------------------------------------------------------------------

typedef struct {
	size_t len;
	size_t cap;
	char buf[]; // flexible array member
} DA_Header;


// get the metadata of the array which is stored before the actual buffer in memory
#define da__header(b) ((DA_Header*)((char*)b - offsetof(DA_Header, buf)))
// checks if n new elements will fit in the array
#define da__fits(b, n) (da_lenu(b) + (n) <= da_cap(b)) 
// if n new elements will not fit in the array, grow the array by reallocating 
#define da__fit(b, n) (da__fits(b, n) ? 0 : ((b) = da__grow((b), da_lenu(b) + (n), sizeof(*(b)))))

#define BUF(x) x // annotates that x is a stretchy buffer
#define da_len(b)  ((b) ? (int32_t)da__header(b)->len : 0)
#define da_lenu(b) ((b) ?          da__header(b)->len : 0)
#define da_set_len(b, l) da__header(b)->len = (l)
#define da_cap(b) ((b) ? da__header(b)->cap : 0)
#define da_end(b) ((b) + da_lenu(b))
#define da_push(b, ...) (da__fit(b, 1), (b)[da__header(b)->len++] = (__VA_ARGS__))
#define da_free(b) ((b) ? (free(da__header(b)), (b) = NULL) : 0)
#define da_printf(b, ...) ((b) = da__printf((b), __VA_ARGS__))

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

char *da__printf(char *buf, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int add_size = 1 + vsnprintf(NULL, 0, fmt, args);
    va_end(args);

	int cur_len = da_len(buf);

	da__fit(buf, add_size);

	char *start = cur_len ? buf + cur_len - 1 : buf;
    va_start(args, fmt);
    vsnprintf(start, add_size, fmt, args);
    va_end(args);

	// if appending to a string that is already null terminated, we clobber the
	// original null terminator so we need to subtract 1
	da__header(buf)->len += cur_len ? add_size - 1 : add_size;

	return buf;
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

	// da_printf
    char *str = NULL;
    da_printf(str, "One: %d\n", 1);
    assert(strcmp(str, "One: 1\n") == 0);
    da_printf(str, "Hex: 0x%x\n", 0x12345678);
	printf("%s", str);
    assert(strcmp(str, "One: 1\nHex: 0x12345678\n") == 0);

}


// Arena Allocator
// ---------------------------------------------------------------------------
#define ARENA_BLOCK_SIZE 65536

typedef struct {
    char *ptr;
    char *end;
    BUF(char **blocks);
} Arena;

void arena_grow(Arena *arena, size_t min_size) {
    size_t size = MAX(ARENA_BLOCK_SIZE, min_size);
    arena->ptr = xmalloc(size);
    arena->end = arena->ptr + size;
    da_push(arena->blocks, arena->ptr);
}

void *arena_alloc(Arena *arena, size_t size) {
    if (arena->ptr + size > arena->end) {
        arena_grow(arena, size); 
    }
    void *ptr = arena->ptr;
    arena->ptr += size;
    return ptr;
}

void *arena_alloc_zeroed(Arena *arena, size_t size) {
    void *ptr = arena_alloc(arena, size);
    memset(ptr, 0, size);
    return ptr;
}

void arena_free(Arena *arena) {
    for (int i=0; i<da_len(arena->blocks); ++i) {
        free(arena->blocks[i]);
    }
    da_free(arena->blocks);
}

void *arena_memdup(Arena *arena, void *src, size_t size) {
    if (size == 0) return NULL;
    void *new_mem = arena_alloc(arena, size);
    memcpy(new_mem, src, size);
    return new_mem;
}


// Hash Map
// ---------------------------------------------------------------------------

typedef struct {
	void **keys;
	void **vals;
	size_t len;
	size_t cap;
} Map;

uint64_t uint64_hash(uint64_t x) {
	x ^= (x * 0xff51afd7ed558ccdull) >> 32;
	return x;
}

uint64_t ptr_hash(void *ptr) {
	return uint64_hash((uintptr_t)ptr);
}

uint64_t str_hash_range(char *start, char *end) {
	uint64_t fnv_init = 0xcbf29ce484222325ull;
	uint64_t fnv_prime = 0x00000100000001B3ull;
	uint64_t hash = fnv_init;
	while (start != end) {
		hash ^= *start++;
		hash *= fnv_prime;
		hash ^= hash >> 32; // additional mixing
	}
	return hash;
}

void *map_get(Map *map, void *key) {
	if (map->len == 0) {
		return NULL;
	}
	assert(map->len < map->cap);
	size_t i = (size_t)ptr_hash(key);

	for (;;) {
		i &= map->cap - 1; // power of two masking
		if (map->keys[i] == NULL) 
			return NULL;
		if (map->keys[i] == key)
			return map->vals[i];
		++i;
	}
}

void map_put(Map *map, void *key, void *val);

void map_grow(Map *map, size_t new_cap) {
	new_cap = MAX(16, new_cap);
	assert(IS_POW2(new_cap));
	Map new_map = {
		.keys = xcalloc(new_cap, sizeof(void*)),
		.vals = xmalloc(new_cap * sizeof(void*)),
		.cap = new_cap,
	};

	for (size_t i = 0; i < map->cap; ++i) {
		if (map->keys[i]) {
			map_put(&new_map, map->keys[i], map->vals[i]);
		}
	}

	free(map->keys);
	free(map->vals);

	*map = new_map;
}

void map_put(Map *map, void *key, void *val) {
	assert(key && val);
	// TODO(shaw): currently enforcing less than 50% capacity, tweak this to be
	// less extreme/conservative
	if (2*map->len >= map->cap) {
		map_grow(map, 2*map->cap);
	}
	assert(2*map->len < map->cap); 

	size_t i = (size_t)ptr_hash(key);

	for (;;) {
		i &= map->cap - 1;
		if (map->keys[i] == NULL) { 
			map->keys[i] = key;
			map->vals[i] = val;
			++map->len;
			return;
		}
		if (map->keys[i] == key) {
			map->vals[i] = val;
			return;
		}
		++i;
	}

}

void map_clear(Map *map) {
	free(map->keys);
	free(map->vals);
	memset(map, 0, sizeof(*map));
}

void map_test(void) {
	Map map = {0};
	enum { N = 1024 * 1024 };
	for (size_t i=0; i<N; ++i) {
		map_put(&map, (void*)(i+1), (void*)(i+2));
	}
	for (size_t i=0; i<N; ++i) {
		assert(map_get(&map, (void*)(i+1)) == (void*)(i+2));
	}
}


// String Interning
// ---------------------------------------------------------------------------
typedef struct InternStr InternStr;
struct InternStr {
    size_t len;
	InternStr *next;
    char str[];
};

static Arena intern_arena;
static Map interns;

char *str_intern_range(char *start, char *end) {
	size_t len = end - start;
	uint64_t hash = str_hash_range(start, end);
	void *key = (void*)(uintptr_t)(hash ? hash : 1);

	// check if string is already interned
	InternStr *intern = map_get(&interns, key);
	// NOTE: in almost all cases this loop should only execute a single time
	for (InternStr *it = intern; it; it = it->next) {
		if (it->len == len && strncmp(it->str, start, len) == 0) {
			return it->str;
		} 
	}

	InternStr *new_intern = arena_alloc(&intern_arena, offsetof(InternStr, str) + len + 1);
	new_intern->len = len;
	new_intern->next = intern;
	memcpy(new_intern->str, start, len);
	new_intern->str[len] = 0;
	map_put(&interns, key, new_intern);
	return new_intern->str;
}

char *str_intern(char *str) {
    // FIXME(shaw): just wrapping the range version for now with a wasteful
    // call to strlen. can be smarter about this but its fine for now.
    return str_intern_range(str, str + strlen(str));
}

void str_intern_test(void) {
	char a[] = "hello";
	assert(strcmp(a, str_intern(a)) == 0);
	assert(str_intern(a) == str_intern(a));
	assert(str_intern(str_intern(a)) == str_intern(a));
	char b[] = "hello";
	assert(a != b);
	assert(str_intern(a) == str_intern(b));
	char c[] = "hello!";
	assert(str_intern(a) != str_intern(c));
	char d[] = "hell";
	assert(str_intern(a) != str_intern(d));

    char e[] = "boobies";
    char f[] = "boobies!";
    char g[] = "boobies";

    char *start = g; 
    char *end = g + strlen(g);

    assert(g != e);
    assert(str_intern(g) == str_intern(e));
    assert(str_intern(g) != str_intern(f));
    assert(str_intern(g) == str_intern_range(start, end));


}







