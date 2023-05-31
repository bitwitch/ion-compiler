typedef struct {
	char *filepath;
	int line;
} SourcePos;

void syntax_error_at(SourcePos pos, char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	printf("%s:%d: syntax error: ", pos.filepath, pos.line);
	vprintf(fmt, args);
	printf("\n");
	va_end(args);

	// TODO(shaw): DELETE ME! JUST FOR DEVELOPMENT!
	assert(0);
}
#define syntax_error(...) syntax_error_at(token.pos, __VA_ARGS__)

void semantic_error(SourcePos pos, char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	printf("%s:%d: error: ", pos.filepath, pos.line);
	vprintf(fmt, args);
	printf("\n");
	va_end(args);

	// TODO(shaw): DELETE ME! JUST FOR DEVELOPMENT!
	assert(0);
	exit(1);
}

void print_note(SourcePos pos, char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	printf("%s:%d: note: ", pos.filepath, pos.line);
	vprintf(fmt, args);
	printf("\n");
	va_end(args);
}

void warning(SourcePos pos, char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	printf("%s:%d: warning: ", pos.filepath, pos.line);
	vprintf(fmt, args);
	printf("\n");
	va_end(args);
}
