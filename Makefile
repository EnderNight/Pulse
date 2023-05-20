
# Memo:
# $^: name of all prerequisties
# $@: target name



# Compiler config
CC = g++
CPPFLAGS =
CFLAGS = -Wall -Wextra -ggdb
LDFLAGS =
LDLIBS =


# Project directory structure
SRCDIR = src
BUILDDIR = obj


# Input files
SRC = $(patsubst $(SRCDIR)/%, %, $(wildcard $(SRCDIR)/*.cpp))
OBJ = $(patsubst %, $(BUILDDIR)/%, $(SRC:.cpp=.o))


# Executable name
EXE = pulse


# Rules
pulse: $(OBJ)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(LDLIBS) -o $(EXE) $^


$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp
	@mkdir obj
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(LDLIBS) -c -o $@ $^

.PHONY: clean
clean:
	$(RM) -rf $(BUILDDIR) $(EXE)
