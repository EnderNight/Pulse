
# Memo:
# $^: name of all prerequisties
# $@: target name



# Compiler config
CC = g++
CPPFLAGS =
CFLAGS = -Wall -Wextra -ggdb
LDFLAGS =
LDLIBS =


# Input files
SRC = pulse.cpp
OBJ = $(SRC:.cpp=.o)


# Executable name
EXE = pulse


# Rules
pulse: $(OBJ)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(LDLIBS) -o $(EXE) $^


$(OBJ): $(SRC)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(LDLIBS) -c -o $@ $^

.PHONY: clean
clean:
	$(RM) -rf $(OBJ) $(EXE)
