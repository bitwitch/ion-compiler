#include <stdbool.h>
#include <ctype.h>
#include <SDL2/SDL.h>

int sdl_scancode_from_noir_key[NUM_KEYS] = {
    [KEY_RETURN]    = SDL_SCANCODE_RETURN,
    [KEY_SPACE]     = SDL_SCANCODE_SPACE,
    [KEY_BACKSPACE] = SDL_SCANCODE_BACKSPACE,
    [KEY_TAB]       = SDL_SCANCODE_TAB,
    [KEY_ESCAPE]    = SDL_SCANCODE_ESCAPE,
    [KEY_LEFT]      = SDL_SCANCODE_LEFT,
    [KEY_RIGHT]     = SDL_SCANCODE_RIGHT,
    [KEY_UP]        = SDL_SCANCODE_UP,
    [KEY_DOWN]      = SDL_SCANCODE_DOWN,
    [KEY_LSHIFT]    = SDL_SCANCODE_LSHIFT,
    [KEY_RSHIFT]    = SDL_SCANCODE_RSHIFT,
    [KEY_LCTRL]     = SDL_SCANCODE_LCTRL,
    [KEY_RCTRL]     = SDL_SCANCODE_RCTRL,
    [KEY_LALT]      = SDL_SCANCODE_LALT,
    [KEY_RALT]      = SDL_SCANCODE_RALT,
};

// TODO(shaw): this is a temporary solution, currently for draw_rect()
SDL_Window *active_window;

int noir_key_from_sdl_scancode[SDL_NUM_SCANCODES];

bool init(void) {
	if (SDL_Init(SDL_INIT_EVERYTHING) < 0) {
		return false;
    }

	// init key tables
	for (int c = 0; c < 256; ++c) {
		// initialize ascii scancodes
		if (isprint(c)) {
			char str[] = {c, 0};
			SDL_Scancode scancode = SDL_GetScancodeFromName(str);
			if (scancode != SDL_SCANCODE_UNKNOWN) {
				sdl_scancode_from_noir_key[(unsigned char)c] = scancode;
			}
		}
	}
	for (int key = 0; key < NUM_KEYS; key++) {
		int scancode = sdl_scancode_from_noir_key[key];
		if (scancode) {
			noir_key_from_sdl_scancode[scancode] = key;
		}
	}

	return true;
}

Window create_window(char *title, Int2 pos, Int2 size) {
	Window window = {0};
	int x = pos.x < 0 ? SDL_WINDOWPOS_CENTERED : pos.x;
	int y = pos.y < 0 ? SDL_WINDOWPOS_CENTERED : pos.y;
    window.sdl_window = SDL_CreateWindow(title, x, y, size.x, size.y, 0);

	// TODO: This is TEMP
	active_window = window.sdl_window;

	return window;
}

void update(void) {
	SDL_PumpEvents();

	const uint8_t *sdl_key_state = SDL_GetKeyboardState(NULL);
	for (int i=0; i<NUM_KEYS; ++i) {
		bool is_down = sdl_key_state[i];
		int k = noir_key_from_sdl_scancode[i];
		bool was_down = keys[k].down;
		keys[k].down = is_down;
		keys[k].pressed = is_down && !was_down;
		keys[k].released = !is_down && was_down;
	}
}

void destroy_window(Window window) {
	SDL_DestroyWindow(window.sdl_window);
}

void draw_rect(int x, int y, int w, int h, uint32_t color) {
	if (!active_window) {
		fprintf(stderr, "Warning: draw_rect() called but there is no active_window\n");
		return;
	}

	SDL_Renderer *renderer = SDL_GetRenderer(active_window);
	SDL_SetRenderDrawColor(renderer,
		(color >> 24) & 0xFF,
		(color >> 16) & 0xFF,
		(color >>  8) & 0xFF,
		(color >>  0) & 0xFF);
	SDL_Rect rect = {x,y,w,h};
	SDL_RenderFillRect(renderer, &rect);
}
