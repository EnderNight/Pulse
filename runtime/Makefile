CC=gcc
CPPFLAGS=
CFLAGS=-Wall -Wextra -std=gnu23 -pedantic
LDFLAGS=

SRC=runtime.c

OBJ=${SRC:.c=.o}

LIB=libpulsert.so


.PHONY: all $(LIB) clean


all: $(LIB)

$(LIB): $(OBJ)
	gcc -o $@ $^ -shared

clean:
	$(RM) $(LIB) $(OBJ)
