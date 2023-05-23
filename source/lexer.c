#include "lexer.h"
#include "types.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Lexer *create_lexer(char *source) {

    Lexer *lexer = malloc(sizeof(Lexer));
    size_t len = strlen(source) + 1;

    lexer->source = malloc(sizeof(char) * len);
    lexer->index = -1;
    lexer->cur_char = NULL;
    lexer->length = len;

    strncpy(lexer->source, source, len);

    next_char(lexer);

    return lexer;
}

void next_char(Lexer *lexer) {

    ++lexer->index;

    if (lexer->index < lexer->length)
        lexer->cur_char = &lexer->source[lexer->index];
    else
        lexer->cur_char = NULL;
}

void previous_char(Lexer *lexer) {

    if (lexer->index > lexer->length)
        lexer->index = lexer->length;

    --lexer->index;
    lexer->cur_char = &lexer->source[lexer->index];
}


int lex_digit(Lexer *lexer) {

    int num = 0;

    while (lexer->cur_char != NULL && isdigit(*lexer->cur_char)) {
        num = num * 10 + (*lexer->cur_char - '0');
        next_char(lexer);
    }
    previous_char(lexer);
    return num;
}


Tokens *lex(Lexer *lexer) {

    Tokens *tokens = create_tokens();
    int value;

    while (lexer->cur_char != NULL) {

        switch (*lexer->cur_char) {

        case '+':
            add_token(tokens, TT_PLUS, 0);
            break;
        case '-':
            add_token(tokens, TT_MINUS, 0);
            break;
        case '*':
            add_token(tokens, TT_MULT, 0);
            break;
        case '/':
            add_token(tokens, TT_DIV, 0);
            break;
        case '%':
            add_token(tokens, TT_MOD, 0);
            break;
        default:
            if (isdigit(*lexer->cur_char)) {
                value = lex_digit(lexer);
                add_token(tokens, TT_INT, value);
            } else if (!isspace(*lexer->cur_char) &&
                       !iscntrl(*lexer->cur_char)) {
                printf("Unrecognized token");
            }
            break;
        }

        next_char(lexer);
    }

    return tokens;
}




void free_lexer(Lexer *lexer) {

    free(lexer->source);

    free(lexer);
}
