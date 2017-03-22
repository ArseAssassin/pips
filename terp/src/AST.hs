module AST where

data ASTNode =
    Expression [ASTNode] |
    ExpressionLiteral ASTNode |
    Term [ASTNode] |
    Lookup String |
    StringLiteral String |
    NumLiteral Int deriving (Show)

