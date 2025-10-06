CC=gcc
CPPFLAGS=
CFLAGS=-Wall -Wextra -std=gnu23 -pedantic -fPIC
LDFLAGS=

SRC=io.c array.c

OBJ=${SRC:.c=.o}

LIB=libpulsert.so


.PHONY: all $(LIB) clean


all: $(LIB)

$(LIB): $(OBJ)
	gcc -o $@ $^ -fPIC -shared

clean:
	$(RM) $(LIB) $(OBJ)
