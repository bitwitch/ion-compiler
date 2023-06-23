#include <dirent.h>

// return a stretchy buf of directory entries
DirEntry *read_dir(char *path) {
	BUF(DirEntry *entries) = NULL;

	DIR *dir = opendir(path);
	if (!dir) {
		return NULL;
	}

	struct dirent *d = NULL;

	while ((d = readdir(dir)) != NULL) {
		if (0 == strcmp(d->d_name, ".") || 0 == strcmp(d->d_name, "..")) {
			continue;
		}
		DirEntry entry = {0};
		path_copy(entry.base, path);
		// entry.size = d->d_reclen;
		memcpy(entry.name, d->d_name, sizeof(entry.name) - 1);
		entry.name[MAX_PATH - 1] = 0;
		entry.is_dir = d->d_type & DT_DIR;
		da_push(entries, entry);
	}

	return entries;
}



