#include "parser.h"
#include "types.h"

#include <stdlib.h>


Parser *create_parser(Tokens *tokens) {

    Parser *parser = malloc(sizeof(Parser));

    parser->tokens = copy_tokens(tokens);
    parser->length = tokens->length;
    parser->index = -1;
    parser->cur_token = NULL;

    next_token(parser);

    return parser;
}

void next_token(Parser *parser) {

    ++parser->index;

    if (parser->index < parser->length)
        parser->cur_token = parser->tokens->tokens[parser->index];
    else
        parser->cur_token = NULL;
}


AST *p_literal(Parser *parser) {

    if (parser->cur_token->type == TT_PLUS ||
        parser->cur_token->type == TT_MINUS) {

        Token *tmp = parser->cur_token;
        next_token(parser);
        AST *num_node = p_literal(parser);
        
        AST *un_op_node = create_un_op_node(tmp, num_node);

        return un_op_node;
    }

    AST *num_node = create_num_node(parser->cur_token);
    next_token(parser);

    return num_node;
}

AST *p_factor(Parser *parser) {

    AST *left = p_literal(parser), *right;
    Token *tmp;

    while (parser->cur_token != NULL && (parser->cur_token->type == TT_MULT ||
                                         parser->cur_token->type == TT_DIV ||
                                         parser->cur_token->type == TT_MOD)) {

        tmp = parser->cur_token;
        next_token(parser);
        right = p_literal(parser);

        left = create_bin_op_node(tmp, left, right);
    }

    return left;
}

AST *p_expr(Parser *parser) {

    AST *left = p_factor(parser), *right;
    Token *tmp;

    while (parser->cur_token != NULL && (parser->cur_token->type == TT_PLUS ||
                                         parser->cur_token->type == TT_MINUS)) {

        tmp = parser->cur_token;
        next_token(parser);
        right = p_factor(parser);

        left = create_bin_op_node(tmp, left, right);
    }

    return left;
}



AST *parse(Parser *parser) { return p_expr(parser); }




void free_parser(Parser *parser) {

    free_tokens(parser->tokens);

    free(parser);
}
