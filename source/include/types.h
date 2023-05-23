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
