#pragma once

#include "types.h"

#include <stddef.h>

typedef struct {
    char *source;
    char *cur_char;
    size_t index;
    size_t length;
} Lexer;

Lexer *create_lexer(char *source);
void next_char(Lexer *lexer);
Tokens *lex(Lexer *lexer);
void free_lexer(Lexer *lexer);
