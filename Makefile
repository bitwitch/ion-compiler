CC := gcc
CFLAGS := -Wall -Wextra -pedantic -Wno-switch -Wno-missing-braces -g -Og -std=c11
LFLAGS := -lm
# NOTE(shaw): c11 is only used for anonymous unions, otherwise everything is c99

.PHONY: ion
ion: main.c
	$(CC) $(CFLAGS) -o ion main.c $(LFLAGS)

.PHONY: solitaire_mac
solitaire_mac:
	./ion solitaire
	$(CC) -I/Library/Frameworks/SDL2.framework/Headers -F/Library/Frameworks -framework SDL2 -o solitaire_mac solitaire.c
