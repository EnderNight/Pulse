#include "token.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*
 * Support only for TT_INT tokens
 */
Token *create_token(TokenType type, int value) {

    Token *token = malloc(sizeof(Token));

    token->value.val_int = value;
    token->type = type;

    return token;
}

Token *copy_token(Token *token) {

    Token *res = malloc(sizeof(Token));

    res->type = token->type;
    res->value = token->value;

    return res;
}

Tokens *copy_tokens(Tokens *tokens) {

    Tokens *res = malloc(sizeof(Tokens));

    res->length = tokens->length;
    res->is_empty = tokens->is_empty;

    res->tokens = malloc(sizeof(Token *) * tokens->length);

    for (size_t i = 0; i < tokens->length; ++i)
        res->tokens[i] = copy_token(tokens->tokens[i]);

    return res;
}

Tokens *create_tokens(void) {

    Tokens *tokens = malloc(sizeof(Tokens));

    tokens->tokens = malloc(sizeof(Token *));
    tokens->length = 1;
    tokens->is_empty = true;

    return tokens;
}



void add_token(Tokens *tokens, TokenType type, int value) {

    if (!tokens->is_empty) {
        ++tokens->length;
        tokens->tokens =
            realloc(tokens->tokens, sizeof(Token *) * tokens->length);
    } else
        tokens->is_empty = false;

    tokens->tokens[tokens->length - 1] = create_token(type, value);
}




void print_token(Token *token) {
    switch (token->type) {

    case TT_INT:
        printf("INT:%d", token->value.val_int);
        break;

    case TT_PLUS:
        printf("PLUS");
        break;

    case TT_MINUS:
        printf("MINUS");
        break;

    case TT_MULT:
        printf("MULT");
        break;

    case TT_DIV:
        printf("DIV");
        break;

    default:
        printf("Unreachable");
        break;
    }
}

void print_tokens(Tokens *tokens) {
    printf("[");

    if (!tokens->is_empty) {
        for (size_t i = 0; i < tokens->length - 1; ++i) {
            print_token(tokens->tokens[i]);
            printf(", ");
        }
        print_token(tokens->tokens[tokens->length - 1]);
    }

    printf("]\n");
}


void free_token(Token *token) { free(token); }

void free_tokens(Tokens *tokens) {
    if (!tokens->is_empty) {
        for (size_t i = 0; i < tokens->length; ++i)
            free_token(tokens->tokens[i]);
    }

    free(tokens->tokens);
    free(tokens);
}
