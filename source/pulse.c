#include "ast.h"
#include "interpreter.h"
#include "lexer.h"
#include "parser.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {

    if (argc == 2) {

        Lexer *lexer;
        Tokens *tokens;
        Parser *parser;
        AST *ast;
        int res;
        char *source = argv[1];
        bool is_file = false;

        if ((source = realpath(argv[1], NULL)) != NULL && source) {
            FILE *src = fopen(source, "r");
            size_t length = 0;

            is_file = true;
            free(source);
            fseek(src, 0, SEEK_END);

            length = ftell(src);
            fseek(src, 0, SEEK_SET);
            source = malloc(sizeof(char) * length);

            if (source)
                fread(source, 1, length, src);

            source[length - 1] = '\0';

            fclose(src);
        }

        lexer = create_lexer(source);
        tokens = lex(lexer);
        // print_tokens(tokens);
        free_lexer(lexer);
        if (is_file)
            free(source);

        parser = create_parser(tokens);
        free_tokens(tokens);
        ast = parse(parser);
        free_parser(parser);
        // print_ast(ast);


        res = inter(ast);
        free_ast(ast);

        printf("%d\n", res);
    }

    return 0;
}
