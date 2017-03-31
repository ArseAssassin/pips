module Terp (runScript) where

import Data.Function ((&))

import Text.ParserCombinators.Parsec.Pos (sourceName, sourceLine, sourceColumn, SourcePos)
import AST
import Runtime
import StdLib
import PIPsParser

runScript :: ASTNode -> Scope -> IO PValue
runScript it scope = do
    eval it updatedScope (PScope updatedScope)
    where updatedScope = putInScope (PString "require") require' scope

require' = PFunction (
    \scope [(PString fileName)] value -> do
        s <- readFile fileName
        let parserOutput = parsePIPs fileName s

        case parserOutput of
            Left parserError ->
                return $ PError (PString "RequireError") ("Error while requiring file: " ++ (show parserError))
            Right ast -> runScript ast defaultScope
    )

addSourcePos :: PValue -> SourcePos -> PValue
addSourcePos (PError typeName it) sourcePos =
    PError typeName $
        it ++
        "\n  in " ++
        (sourceName sourcePos) ++
        ":" ++ (show (sourceLine sourcePos)) ++
        ":" ++ (show (sourceColumn sourcePos))

execExpression :: IO (Scope, IO PValue) -> ASTNode -> IO (Scope, IO PValue)
execExpression input astNode = do
    (scope, value) <- input
    value' <- value
    let (updatedScope, updatedValue) = case value' of
                                            PAssignScope it -> (mergeScopes it scope, PScope it)
                                            _ -> (scope, value')

    return (updatedScope, eval astNode updatedScope updatedValue)

callFunction :: Scope -> [PValue] -> PValue -> PValue -> IO PValue
callFunction scope args input evaledFn@(PFunction fn) = do
    newVal <- fn scope args input
    return $ case newVal of
        PError typeName it ->
            case meta [PString "name"] scope evaledFn of
                PError _ _ -> PError typeName $ "Error calling anonymous function: \n" ++ it
                name -> PError typeName $ "Error calling function named " ++ (show name) ++ ": \n" ++ it
        it -> it


eval :: ASTNode -> Scope -> PValue -> IO PValue
eval (Expression nodes sourcePos) scope value = do
    output <- foldl execExpression (return (scope, return value)) nodes
    value <- snd output

    return $ case value of
        it@(PError _ _) -> addSourcePos it sourcePos
        it -> it

eval (Term (fn:args) sourcePos) scope value = do
    evaledArgs <- sequence $ map (\it -> eval it scope value) args
    evaledFn <- eval fn scope value

    let updatedScope = case meta [(PString "accessParentScope")] scope evaledFn of
                            PHashMap vals -> map (\(name, parentName) ->
                                                case findFromScope parentName scope of
                                                    Just it -> (name, it)
                                                    Nothing -> (name, PError (PString "LookupError") $ "Couldn't find value " ++ (show name) ++ " from parent scope")
                                            ) vals
                            _ -> scope
                        & (putInScope (PString "__functionMeta") (meta [] scope evaledFn))


    case unmeta evaledFn of
        fn@(PFunction _) -> callFunction updatedScope evaledArgs value fn
        PError typeName it ->
            return $ addSourcePos (
                case meta [PString "name"] updatedScope evaledFn of
                    PError (PString "LookupError") _ -> PError typeName it
                    name -> PError typeName $ "While calling function " ++ (show name) ++ ": " ++ it
            ) sourcePos
        it -> return $ addSourcePos (PError (PString "ValueError") $ "Calling invalid function " ++ (show it)) sourcePos

eval (ExpressionLiteral astNodes) scope _ =
    return fn
    where fn = PFunction $
                \newScope args value -> do
                    let metaFn = case findFromScope (PString "__functionMeta") newScope of
                                    Just (PHashMap it) -> foldl (\value (name, val) -> PMeta name val value) fn it
                                    Nothing -> fn

                    let updatedScope = [(PString "args", (PList args)),
                                        (PString "input", value),
                                        (PString "recur", metaFn)] ++ newScope ++ scope

                    eval astNodes updatedScope value

eval (Lookup name) scope _ =
    return $ case findFromScope (PString name) scope of
        Just it -> it
        Nothing -> PError (PString "LookupError") $ "Couldn't find value " ++ (show name) ++ " from scope"

eval (NumLiteral i) _ _ = return $ PNum i
eval (StringLiteral s) _ _ = return $ PString s

