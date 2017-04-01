module Terp (runScript) where

import Data.Function ((&))

import Text.ParserCombinators.Parsec.Pos (sourceName, sourceLine, sourceColumn, SourcePos)
import AST
import Runtime
import StdLib
import PIPsParser

runScript :: ASTNode -> Scope -> IO PValue
runScript it scope =
    return $ eval it scope (PScope scope)
    -- where updatedScope = putInScope (PString "require") require' scope

-- require' = PFunction (
--     \scope [(PString fileName)] value -> do
--         s <- readFile fileName
--         let parserOutput = parsePIPs fileName s

--         case parserOutput of
--             Left parserError ->
--                 return $ PError (PString "RequireError") ("Error while requiring file: " ++ (show parserError))
--             Right ast -> runScript ast defaultScope
--     )

addSourcePos :: PValue -> SourcePos -> PValue
addSourcePos (PError typeName it) sourcePos =
    PError typeName $
        it ++
        "\n  in " ++
        (sourceName sourcePos) ++
        ":" ++ (show (sourceLine sourcePos)) ++
        ":" ++ (show (sourceColumn sourcePos))

eval :: ASTNode -> Scope -> PValue -> PValue
eval (Expression nodes sourcePos) scope value =
    snd $ foldl (\(scope, val) astNode ->
                    let (updatedScope, updatedValue) = case val of
                                                            PAssignScope it -> (s, PScope s)
                                                                where s = mergeScopes it scope
                                                            _ -> (scope, val)
                    in (updatedScope, eval astNode updatedScope updatedValue)
                ) (scope, value) nodes

eval (Term (fn:args) sourcePos) scope value =
    let updatedScope = case meta [(PString "accessParentScope")] scope evaledFn of
                            PHashMap vals -> map (\(name, parentName) ->
                                                case findFromScope parentName scope of
                                                    Just it -> (name, it)
                                                    Nothing -> (name, PError (PString "LookupError") $ "Couldn't find value " ++ (show name) ++ " from parent scope")
                                            ) vals
                            _ -> scope
                        & (putInScope (PString "__functionMeta") (meta [] scope evaledFn))
        evaledArgs = map (\it -> eval it scope value) args
        evaledFn = eval fn scope value
        in case unmeta evaledFn of
            PFunction fn ->
                case fn updatedScope evaledArgs value of
                    PError typeName it ->
                        case meta [PString "name"] scope evaledFn of
                            PError _ _ -> PError typeName $ "Error calling anonymous function: \n" ++ it
                            name -> PError typeName $ "Error calling function named " ++ (show name) ++ ": \n" ++ it
                    it -> it
            PError typeName it ->
                addSourcePos (
                    case meta [PString "name"] updatedScope evaledFn of
                        PError (PString "LookupError") _ -> PError typeName it
                        name -> PError typeName $ "While calling function " ++ (show name) ++ ": " ++ it
                ) sourcePos
            it -> addSourcePos (PError (PString "ValueError") $ "Calling invalid function " ++ (show it)) sourcePos

eval (ExpressionLiteral astNode) scope _ =
    fn
    where fn = PFunction $
                \newScope args value ->
                    let metaFn = case findFromScope (PString "__functionMeta") newScope of
                                    Just (PHashMap it) -> foldl (\value (name, val) -> PMeta name val value) fn it
                                    Nothing -> fn
                        updatedScope = [(PString "args", (PList args)),
                                        (PString "input", value),
                                        (PString "recur", fn)] ++ newScope ++ scope
                        in eval astNode updatedScope value

eval (Lookup name) scope _ =
    case findFromScope (PString name) scope of
        Just it -> it
        Nothing -> PError (PString "LookupError") $ "Couldn't find value " ++ (show name) ++ " from scope"

eval (NumLiteral i) _ _ = PNum i
eval (StringLiteral s) _ _ = PString s

