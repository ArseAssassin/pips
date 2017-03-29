module Terp (runScript) where

import Text.ParserCombinators.Parsec.Pos (sourceName, sourceLine, sourceColumn)
import AST
import Runtime
import StdLib

runScript :: ASTNode -> Scope -> PValue
runScript it scope = eval it scope (PScope scope)

eval :: ASTNode -> Function
eval (Expression nodes sourcePos) scope value =
    case output of
        PError typeName it -> PError typeName $
                it ++
                "\n  in " ++
                (sourceName sourcePos) ++
                ":" ++ (show (sourceLine sourcePos)) ++
                ":" ++ (show (sourceColumn sourcePos))
        it -> it

    where
        output = snd $ foldl (
            \(scope, value) astNode ->
                let updatedScope = case value of
                                        PAssignScope it -> it
                                        _ -> scope
                in (updatedScope, eval astNode updatedScope value)
            ) (scope, value) nodes

eval (Term (fn:args)) scope value =
    case unmeta evaledFn of
        PFunction fn ->
            let newVal = fn updatedScope value
            in case newVal of
                PError typeName it ->
                    case meta [PString "name"] updatedScope evaledFn of
                        PError _ _ -> PError typeName $ "Error calling anonymous function: \n" ++ it
                        name -> PError typeName $ "Error calling function named " ++ (show name) ++ ": \n" ++ it
                it -> it
        PError typeName it -> PError typeName it
            -- case meta [PString "name"] updatedScope evaledFn of
        it -> PError (PString "ValueError") $ "Calling invalid function " ++ (show it)
    where
        evaledArgs = map (\it -> eval it scope value) args
        updatedScope = putInScope (PString "input") value $ putInScope (PString "args") (PList evaledArgs) scope
        evaledFn = eval fn updatedScope value

eval (ExpressionLiteral astNodes) scope _ =
    PFunction (
        \newScope value ->
            eval astNodes (mergeScopes newScope scope) value
    )

eval (Lookup name) scope _ =
    case findFromScope (PString name) scope of
        Just it -> it
        Nothing -> PError (PString "LookupError") $ "Couldn't find value " ++ (show name) ++ " from scope"

eval (NumLiteral i) _ _ = PNum i
eval (StringLiteral s) _ _ = PString s

