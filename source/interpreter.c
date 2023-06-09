#include "interpreter.h"
#include "types.h"


int plus(int x, int y) { return x + y; }
int minus(int x, int y) { return x - y; }
int mult(int x, int y) { return x * y; }
int div(int x, int y) { return x / y; }
int mod(int x, int y) { return x % y; }


int inter(AST *ast) {

    if (ast->type == NUM_NODE)
        return ast->node.num_node->token->value.val_int;

    if (ast->type == BIN_OP_NODE) {

        int left_res = inter(ast->node.bin_op_node->left),
            right_res = inter(ast->node.bin_op_node->right), res = 0;

        switch (ast->node.bin_op_node->token->type) {
        case TT_PLUS:
            res = plus(left_res, right_res);
            break;
        case TT_MINUS:
            res = minus(left_res, right_res);
            break;
        case TT_MULT:
            res = mult(left_res, right_res);
            break;
        case TT_DIV:
            res = div(left_res, right_res);
            break;
        case TT_MOD:
            res = mod(left_res, right_res);
            break;
        default:
            break;
        }

        return res;
    }

    if (ast->type == UN_OP_NODE) {
        int res = inter(ast->node.un_op_node->operand);

        if (ast->node.un_op_node->token->type == TT_MINUS)
            res *= -1;

        return res;
    }

    return 0;
}
