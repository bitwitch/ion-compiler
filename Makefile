CC := gcc
CFLAGS := -Wall -Wextra -pedantic -Wno-switch -Wno-missing-braces -g -Og -std=c11
LFLAGS := -lm
# NOTE(shaw): c11 is only used for anonymous unions, otherwise everything is c99

.PHONY: ion
ion: main.c
	$(CC) $(CFLAGS) -o ion main.c $(LFLAGS)
