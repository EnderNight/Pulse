#pragma once

#include <stdbool.h>
#include <stddef.h>

typedef enum {
    TT_INT,

    TT_PLUS,
    TT_MINUS,
    TT_MULT,
    TT_DIV
} TokenType;

typedef union {
    int val_int;
} Value;

typedef struct {
    TokenType type;
    Value value;
} Token;

typedef struct {
    Token **tokens;
    size_t length;
    bool is_empty;
} Tokens;

Token *create_token(TokenType type, int value);
Tokens *create_tokens(void);
Token *copy_token(Token *token);
Tokens *copy_tokens(Tokens *token);
void add_token(Tokens *tokens, TokenType type, int value);
void print_token(Token *token);
void print_tokens(Tokens *tokens);
void free_token(Token *token);
void free_tokens(Tokens *tokens);
