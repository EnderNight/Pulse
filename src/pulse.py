import sys


class TokenType:
    TOKEN_NUM = 0
    TOKEN_PLUS = 1
    TOKEN_MINUS = 2


class Associativity:
    LEFT = 0
    RIGHT = 1
    UNDEFINED = 2


class Valence:
    UNDEFINED = 0
    UNARY = 1
    BINARY = 2
    TERNARY = 3


class Precedence:
    NUM = 0
    PL_MI = 1
    ML_DV = 2
    PO = 3


class Token:

    def __init__(self, typ: int, prec: int,
                 asso: int, val: int, value) -> None:
        self.typ = typ
        self.prec = prec
        self.asso = asso
        self.val = val
        self.value = value

    def __repr__(self) -> str:
        return str(self)

    def __str__(self) -> str:
        return ("T(" + str(self.value) + ")")


def lexer(source: str) -> list[Token]:

    res = []
    sourceList = source.split()

    for word in sourceList:
        if word[0] == "+":
            res.append(Token(TokenType.TOKEN_PLUS, Precedence.PL_MI,
                             Associativity.LEFT, Valence.BINARY, word))
        elif word[0] == "-":
            res.append(Token(TokenType.TOKEN_MINUS, Precedence.PL_MI,
                             Associativity.LEFT, Valence.BINARY, word))
        else:
            res.append(Token(TokenType.TOKEN_NUM, Precedence.NUM,
                             Associativity.UNDEFINED, Valence.UNDEFINED,
                             int(word)))

    return res


def parser(infTokens: list[Token]) -> list[Token]:

    rpnTokens = []
    stack = []

    while len(infTokens) > 0:

        tmp = infTokens.pop(0)

        if tmp.typ == TokenType.TOKEN_NUM:
            rpnTokens.append(tmp)
        else:
            while len(stack) > 0 and (stack[-1].prec > tmp.prec or (
                    stack[-1].prec == tmp.prec
                    and tmp.asso == Associativity.LEFT)):
                rpnTokens.append(stack.pop())
            stack.append(tmp)

    while len(stack) > 0:
        rpnTokens.append(stack.pop())

    return rpnTokens


def plus(x: int, y: int) -> int:
    return x + y


def minus(x: int, y: int) -> int:
    return x - y


def executor(rpnTokens: list[Token]) -> int:

    tmpStack = []

    while len(rpnTokens) > 0:

        tmp = rpnTokens.pop(0)

        if tmp.typ == TokenType.TOKEN_NUM:
            tmpStack.append(tmp.value)
        else:
            tmpOp = []
            for _ in range(tmp.val):
                tmpOp.append(tmpStack.pop())

            if tmp.typ == TokenType.TOKEN_PLUS:
                tmpStack.append(plus(tmpOp[0], tmpOp[1]))
            elif tmp.typ == TokenType.TOKEN_MINUS:
                tmpStack.append(minus(tmpOp[1], tmpOp[0]))

    return tmpStack[0]


if __name__ == '__main__':

    if len(sys.argv) != 2:
        assert False, ("Incorrect number of arguments, you must only provide "
                       "a single path to an .pul file")

    input = open(sys.argv[1], 'r')

    infTokens = lexer(input.read())

    rpnTokens = parser(infTokens)

    print(executor(rpnTokens))
