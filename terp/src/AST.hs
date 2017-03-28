module AST where

import Text.ParserCombinators.Parsec.Pos (SourcePos)

data ASTNode =
    Expression [ASTNode] SourcePos |
    ExpressionLiteral ASTNode |
    Term [ASTNode] |
    Lookup String |
    StringLiteral String |
    NumLiteral Int deriving (Show)

