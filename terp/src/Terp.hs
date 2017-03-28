module Terp (runScript) where

import AST
import Runtime
import StdLib

runScript :: ASTNode -> PValue
runScript it = eval it defaultScope (PScope defaultScope)

eval :: ASTNode -> Function
eval (Expression nodes) scope value =
    snd $ foldl (\(scope, value) astNode ->
        case eval astNode scope value of
            PAssignScope scope -> (scope, value)
            it -> (scope, it)
        ) (scope, value) nodes

eval (Term (fn:args)) scope value =
    case eval fn updatedScope value of
        PFunction fn -> fn updatedScope value
        PError it -> PError it
        it -> PError $ "Calling invalid function " ++ (show it)
    where
        evaledArgs = map (\it -> eval it scope value) args
        updatedScope = putInScope (PString "it") value $ putInScope (PString "args") (PList evaledArgs) scope

eval (ExpressionLiteral astNodes) scope _ =
    PFunction (
        \newScope value ->
            eval astNodes (mergeScopes newScope scope) value
    )

eval (Lookup name) scope value =
    case findFromScope (PString name) scope of
        Just it -> it
        Nothing -> PError $ "Couldn't find value " ++ (show name) ++ " from scope"

eval (NumLiteral i) _ _ = PNum i
eval (StringLiteral s) _ _ = PString s

