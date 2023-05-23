#include "types.h"

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

    case TT_MOD:
        printf("MOD");
        break;

    case TT_LPAREN:
        printf("LPAREN");
        break;

    case TT_RPAREN:
        printf("RPAREN");
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


AST *create_ast(void) {

    AST *res = malloc(sizeof(AST));

    return res;
}

AST *create_num_node(Token *token) {

    AST *ast = create_ast();

    ast->node.num_node = malloc(sizeof(NumNode));
    ast->node.num_node->token = copy_token(token);
    ast->type = NUM_NODE;

    return ast;
}

AST *create_bin_op_node(Token *token, AST *left, AST *right) {

    AST *ast = create_ast();

    ast->node.bin_op_node = malloc(sizeof(BinOpNode));
    ast->node.bin_op_node->token = copy_token(token);
    ast->node.bin_op_node->left = left;
    ast->node.bin_op_node->right = right;
    ast->type = BIN_OP_NODE;

    return ast;
}

AST *create_un_op_node(Token *token, AST *operand) {

    AST *ast = create_ast();

    ast->type = UN_OP_NODE;
    ast->node.un_op_node = malloc(sizeof(UnOpNode));
    ast->node.un_op_node->operand = operand;
    ast->node.un_op_node->token = copy_token(token);

    return ast;
}

void _print_ast(AST *ast) {

    printf("(");


    if (ast->type == NUM_NODE) {
        print_token(ast->node.num_node->token);
    } else if (ast->type == BIN_OP_NODE) {
        _print_ast(ast->node.bin_op_node->left);
        printf(" ");
        print_token(ast->node.bin_op_node->token);
        printf(" ");
        _print_ast(ast->node.bin_op_node->right);
    } else if (ast->type == UN_OP_NODE) {
        print_token(ast->node.un_op_node->token);
        printf(" ");
        _print_ast(ast->node.un_op_node->operand);
    } else {
        printf("unreachable");
    }


    printf(")");
}

void print_ast(AST *ast) {
    _print_ast(ast);
    printf("\n");
}

void free_numnode(NumNode *node) {
    free_token(node->token);
    free(node);
}

void free_binopnode(BinOpNode *node) {
    free_token(node->token);

    free_ast(node->left);
    free_ast(node->right);
    free(node);
}

void free_un_op_node(UnOpNode *node) {
    free_token(node->token);

    free_ast(node->operand);
    free(node);
}

void free_ast(AST *ast) {

    if (ast->type == NUM_NODE) {
        free_numnode(ast->node.num_node);
    } else if (ast->type == BIN_OP_NODE) {
        free_binopnode(ast->node.bin_op_node);
    } else if (ast->type == UN_OP_NODE) {
        free_un_op_node(ast->node.un_op_node);
    }

    free(ast);
}
