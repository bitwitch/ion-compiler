CC := gcc
CFLAGS := -Wall -Wextra -pedantic -g -Og -std=c11
# NOTE(shaw): c11 is only used for anonymous unions, otherwise everything is c99

.PHONY: ion
ion: ion.c
	$(CC) $(CFLAGS) -o ion ion.c
