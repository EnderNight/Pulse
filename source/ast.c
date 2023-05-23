#include "ast.h"
#include "token.h"

#include <stdio.h>
#include <stdlib.h>

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

void _print_ast(AST *ast, bool is_root) {

    printf("(");


    if (ast->type == NUM_NODE) {
        print_token(ast->node.num_node->token);
    } else if (ast->type == BIN_OP_NODE) {
        _print_ast(ast->node.bin_op_node->left, false);
        printf(" ");
        if (is_root)
            printf("\e[0;31m");
        print_token(ast->node.bin_op_node->token);
        if (is_root)
            printf("\e[0m");
        printf(" ");
        _print_ast(ast->node.bin_op_node->right, false);
    } else {
        printf("unreachable");
    }


    printf(")");
}

void print_ast(AST *ast) {
    _print_ast(ast, true);
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

void free_ast(AST *ast) {

    if (ast->type == NUM_NODE) {
        free_numnode(ast->node.num_node);
    } else if (ast->type == BIN_OP_NODE) {
        free_binopnode(ast->node.bin_op_node);
    }

    free(ast);
}
