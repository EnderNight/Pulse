# Project constants
EXE = pulse

# Directory variables
SRCDIR = source
BUILDDIR = build
INCDIR = $(SRCDIR)/include


# Compiler flags
CC = gcc
CPPFLAGS = -I$(INCDIR) -MMD
CFLAGS = -Wall -Wextra -std=c2x -pedantic -ggdb
LDFLAGS = -fsanitize=address
LIBS =


# File variables
SRC = $(wildcard $(SRCDIR)/*.c)
OBJ = $(patsubst $(SRCDIR)/%.o, $(BUILDDIR)/%.o, $(SRC:.c=.o))
HDR = $(wildcard $(INCDIR)/*.h)


all: $(EXE) install


# Main executable compilation
$(EXE): $(OBJ)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(LIBS) -o $(EXE) $^

# Objects compilation
$(BUILDDIR)/%.o: $(SRCDIR)/%.c $(HDR)
	@mkdir -p $(BUILDDIR)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(LIBS) -c -o $@ $<


.PHONY: clean install uninstall

clean:
	$(RM) -rf $(BUILDDIR) $(EXE)

install:
	@echo "Installing pulse in $(HOME)/.local/bin/"
	@cp pulse $(HOME)/.local/bin/

uninstall:
	@echo "Uninstalling pulse from $(HOME)/.local/bin/"
	$(RM) -f $(HOME)/.local/bin/pulse
