module PIPsParser (parsePIPs) where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM2, liftM)
import AST

manyTill1 :: GenParser tok st a -> GenParser tok st end -> GenParser tok st [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

termSeparator = char ','
expressionSeparator = char ')'

valueSeparator = (try space) <|> (lookAhead termSeparator) <|> (lookAhead expressionSeparator)

anySeparator = valueSeparator <|> termSeparator <|> expressionSeparator

sourcePos = statePos `liftM` getParserState

num = manyTill1 digit valueSeparator

number :: GenParser Char st ASTNode
number = do
    content <-
        ((:) <$> (try $ char '-') <*> num) <|> num

    return $ NumLiteral (read content)

word :: GenParser Char st ASTNode
word = do
    char '\''
    content <- manyTill anyChar valueSeparator
    return $ StringLiteral content

stringBlock :: GenParser Char st ASTNode
stringBlock = do
    char '"'
    content <- manyTill anyChar (char '"')
    spaces
    return $ StringLiteral content

stringLiteral = (try stringBlock) <|> (try word)

scopeLookup :: GenParser Char st ASTNode
scopeLookup = do
    content <- manyTill1 (noneOf ",)") valueSeparator
    return $ Lookup content

value :: GenParser Char st ASTNode
value = subexpression <|> stringLiteral <|> number <|> scopeLookup

term :: GenParser Char st ASTNode
term = do
    spaces
    content <- sepBy1 value spaces
    return $ Term content

expression :: GenParser Char st ASTNode
expression = do
    pos <- sourcePos
    content <- sepBy1 term termSeparator
    spaces
    return $ Expression content pos


expressionLiteral :: GenParser Char st ASTNode
expressionLiteral = do
    char '('
    content <- expression
    char ')'
    spaces
    return content

valueExpression :: GenParser Char st ASTNode
valueExpression = do
    string "'("
    content <- expression
    char ')'
    spaces
    return $ ExpressionLiteral content

subexpression = (try expressionLiteral) <|> (try valueExpression)

parsePIPs name it = parse expression name it
