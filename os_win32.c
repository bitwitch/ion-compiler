#include <io.h>

// return a stretchy buf of directory entries
DirEntry *read_dir(char *path) {
	BUF(DirEntry *entries) = NULL;

    char filespec[MAX_PATH];
    path_copy(filespec, path);
    path_join(filespec, "*");

	intptr_t handle;
    struct _finddata_t fileinfo;

    handle = _findfirst(filespec, &fileinfo);
	if (handle == -1) {
		// no files
		return NULL;
	}

	do {
		DirEntry entry = {0};
		path_copy(entry.base, path);
		entry.size = fileinfo.size;
		memcpy(entry.name, fileinfo.name, sizeof(entry.name) - 1);
		entry.name[MAX_PATH - 1] = 0;
		entry.is_dir = fileinfo.attrib & _A_SUBDIR;
		da_push(entries, entry);
	} while (_findnext(handle, &fileinfo) == 0);

	return entries;
}
