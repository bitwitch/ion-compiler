typedef struct {
	char *filepath;
	int line;
} SourcePos;

SourcePos pos_builtin = {.filepath = "<builtin>"};

void syntax_error_at(SourcePos pos, char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "%s:%d: syntax error: ", pos.filepath, pos.line);
	vfprintf(stderr, fmt, args);
	fprintf(stderr, "\n");
	va_end(args);

	// TODO(shaw): DELETE ME! JUST FOR DEVELOPMENT!
	assert(0);
}
#define syntax_error(...) syntax_error_at(token.pos, __VA_ARGS__)

void semantic_error(SourcePos pos, char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "%s:%d: error: ", pos.filepath, pos.line);
	vfprintf(stderr, fmt, args);
	fprintf(stderr, "\n");
	va_end(args);

	// TODO(shaw): DELETE ME! JUST FOR DEVELOPMENT!
	assert(0);
}

void fatal_semantic_error(SourcePos pos, char *fmt) {
	semantic_error(pos, fmt);
	exit(1);
}

void print_note(SourcePos pos, char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "%s:%d: note: ", pos.filepath, pos.line);
	vfprintf(stderr, fmt, args);
	fprintf(stderr, "\n");
	va_end(args);
}

void warning(SourcePos pos, char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "%s:%d: warning: ", pos.filepath, pos.line);
	vfprintf(stderr, fmt, args);
	fprintf(stderr, "\n");
	va_end(args);
}
