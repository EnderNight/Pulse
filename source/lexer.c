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

char *lex_identifier(Lexer *lexer) {

    char *res;

}



Tokens *lex(Lexer *lexer) {

    Tokens *tokens = create_tokens();
    Value value;

    while (lexer->cur_char != NULL) {

        switch (*lexer->cur_char) {

        case '+':
            add_token(tokens, TT_PLUS, value);
            break;
        case '-':
            add_token(tokens, TT_MINUS, value);
            break;
        case '*':
            add_token(tokens, TT_MULT, value);
            break;
        case '/':
            add_token(tokens, TT_DIV, value);
            break;
        case '%':
            add_token(tokens, TT_MOD, value);
            break;
        case '^':
            add_token(tokens, TT_POW, value);
            break;
        case '(':
            add_token(tokens, TT_LPAREN, value);
            break;
        case ')':
            add_token(tokens, TT_RPAREN, value);
            break;
        case '=':
            add_token(tokens, TT_EQ, value);
            break;
        default:
            if (isdigit(*lexer->cur_char)) {
                value.val_int = lex_digit(lexer);
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
