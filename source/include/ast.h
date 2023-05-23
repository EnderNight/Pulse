#pragma once

#include "token.h"

#include <stddef.h>

typedef struct NumNode NumNode;
typedef struct BinOpNode BinOpNode;

typedef union {

    NumNode *num_node;
    BinOpNode *bin_op_node;

} Node;

typedef enum { NUM_NODE, BIN_OP_NODE } NodeType;

typedef struct {
    Node node;
    NodeType type;
    size_t num_children;
} AST;

typedef struct NumNode {
    Token *token;
} NumNode;

typedef struct BinOpNode {
    Token *token;
    AST *left;
    AST *right;
} BinOpNode;

AST *create_ast(void);
AST *create_num_node(Token *token);
AST *create_bin_op_node(Token *token, AST *left, AST *right);
void print_ast(AST *ast);
void free_num_node(NumNode *node);
void free_bin_op_node(BinOpNode *node);
void free_ast(AST *ast);
