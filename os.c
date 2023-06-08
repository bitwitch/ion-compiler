#ifndef MAX_PATH
#if defined _MAX_PATH
#define MAX_PATH _MAX_PATH
#elif defined PATH_MAX
#define MAX_PATH PATH_MAX
#else
#error "No suitable MAX_PATH surrogate"
#endif
#endif

// path string helpers

void path_normalize(char path[MAX_PATH]) {
    char *ptr;
    for (ptr = path; *ptr; ++ptr) {
        if (*ptr == '\\') {
            *ptr = '/';
        }
    }
	// remove trailing slash
    if (ptr != path && ptr[-1] == '/') {
        ptr[-1] = 0;
    }
}

void path_copy(char path[MAX_PATH], char *src) {
    strncpy(path, src, MAX_PATH);
    path[MAX_PATH - 1] = 0;
    path_normalize(path);
}

char *path_ext(char path[MAX_PATH]) {
	for (int i = strlen(path) - 1; i > 0; --i) {
		if (path[i] == '/' || path[i] == '\\') {
			break;
		} else if (path[i] == '.') {
			return path + i + 1;
		}
	}
	return path;
}

char *replace_ext(char path[MAX_PATH], char *ext) {
	if (!path) return NULL;
	for (int i = strlen(path); i > 0; --i) {
		if (path[i] == '/' || path[i] == '\\') {
			// path has no extension
			return NULL;
		} else if (path[i] == '.') {
			return strf("%.*s.%s", i, path, ext);
		}
	}
	return NULL;
}

void path_join(char path[MAX_PATH], char *src) {
    char *ptr = path + strlen(path);
    if (ptr != path && ptr[-1] == '/') {
        ptr--;
    }
    if (*src == '/') {
        src++;
    }
    snprintf(ptr, path + MAX_PATH - ptr, "/%s", src);
}

// directory reading

typedef struct {
    char base[MAX_PATH];
    char name[MAX_PATH];
    size_t size;
    bool is_dir;
} DirEntry;


// The platform specific implementations below define the following functions:
DirEntry *read_dir(char *path);

#ifdef _MSC_VER
#include "os_win32.c"
#define strdup _strdup
#else
#include "os_unix.c"
#endif


void read_dir_test(void) {
	DirEntry *entries = read_dir("C:/Users/shmow/code/ion-compiler");
	for (int i=0; i<da_len(entries); ++i) {
		printf("Entry[%d]=%s\n", i, entries[i].name);
	}
}
