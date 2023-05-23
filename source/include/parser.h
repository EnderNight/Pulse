#pragma once

#include "ast.h"
#include "token.h"

#include <stddef.h>

typedef struct {
    Tokens *tokens;
    size_t length;
    Token *cur_token;
    size_t index;
} Parser;

Parser *create_parser(Tokens *tokens);
void next_token(Parser *parser);
AST *parse(Parser *parser);
void free_parser(Parser *parser);
