CC := gcc
CFLAGS := -Wall -g -Og -std=c99 

.PHONY: ion
ion: ion.c
	$(CC) $(CFLAGS) -o ion ion.c
