module Terp (runScript) where

import Text.ParserCombinators.Parsec.Pos (sourceName, sourceLine, sourceColumn)
import AST
import Runtime
import StdLib
import PIPsParser

runScript :: ASTNode -> Scope -> IO PValue
runScript it scope = do
    eval it updatedScope (PScope updatedScope)
    where updatedScope = putInScope (PString "require") require' scope

require' = PFunction (
    \scope value ->
        case findFromScope (PString "args") scope of
            Just (PList [PString fileName]) -> do
                s <- readFile fileName
                let parserOutput = parsePIPs fileName s

                case parserOutput of
                    Left parserError ->
                        return $ PError (PString "RequireError") ("Error while requiring file: " ++ (show parserError))
                    Right ast -> runScript ast defaultScope
            _ -> return $ PError (PString "RequireError") "Error while requiring file"
    )

execExpression :: IO (Scope, IO PValue) -> ASTNode -> IO (Scope, IO PValue)
execExpression input astNode = do
    (scope, value) <- input
    value' <- value
    let updatedScope = case value' of
                            PAssignScope it -> it
                            _ -> scope


    return (updatedScope, eval astNode updatedScope value')

callFunction :: Scope -> PValue -> PValue -> IO PValue
callFunction scope input evaledFn@(PFunction fn) = do
    newVal <- fn scope input
    return $ case newVal of
        PError typeName it ->
            case meta [PString "name"] scope evaledFn of
                PError _ _ -> PError typeName $ "Error calling anonymous function: \n" ++ it
                name -> PError typeName $ "Error calling function named " ++ (show name) ++ ": \n" ++ it
        it -> it

eval :: ASTNode -> Function
eval (Expression nodes sourcePos) scope value = do
    output <- foldl execExpression (return (scope, return value)) nodes
    value <- snd output

    return $ case value of
        PError typeName it -> PError typeName $
                it ++
                "\n  in " ++
                (sourceName sourcePos) ++
                ":" ++ (show (sourceLine sourcePos)) ++
                ":" ++ (show (sourceColumn sourcePos))
        it -> it


eval (Term (fn:args)) scope value = do
    evaledArgs <- sequence $ map (\it -> eval it scope value) args
    let updatedScope = putInScope (PString "input") value $ putInScope (PString "args") (PList evaledArgs) scope
    evaledFn <- eval fn updatedScope value

    case unmeta evaledFn of
        fn@(PFunction _) -> callFunction updatedScope value fn
        PError typeName it -> return $ PError typeName it
            -- case meta [PString "name"] updatedScope evaledFn of
        it -> return $ PError (PString "ValueError") $ "Calling invalid function " ++ (show it)

eval (ExpressionLiteral astNodes) scope _ =
    return $ PFunction (
        \newScope value ->
            eval astNodes (mergeScopes newScope scope) value
    )

eval (Lookup name) scope _ =
    return $ case findFromScope (PString name) scope of
        Just it -> it
        Nothing -> PError (PString "LookupError") $ "Couldn't find value " ++ (show name) ++ " from scope"

eval (NumLiteral i) _ _ = return $ PNum i
eval (StringLiteral s) _ _ = return $ PString s

