module Terp (runScript) where

import AST
import Runtime
import StdLib

runScript :: ASTNode -> PValue
runScript node =
    eval node defaultScope (PScope defaultScope)

processValue ((PAssignScope newScope), _) = (PScope newScope, newScope)
processValue ((PRoutine (PGetScope fn)), scope) = processValue ((fn scope), scope)
processValue ((PList (PRoutine (PGetScope fn):xs)), scope) =
    (complete, scope)
    where
        rest = processValue ((PList xs), scope)
        complete =
            PList $ (fn scope) : case rest of
                (PList it, _) -> it

processValue it = it

eval :: ASTNode -> Scope -> PValue -> PValue
eval (Term (fn:args)) scope value =
    case eval fn newScope value of
        PRoutine (PExpression fn') ->
            fn' evaledArgs value
        PRoutine (PFunction fn') ->
            fn' newScope value
        PRoutine (PGetScope fn') ->
            fn' newScope
        PError it -> PError it
        it -> PError ("Calling invalid function" ++ (show it))
    where
        evaledArgs = map (\astNode -> eval astNode scope value) args
        newScope = (putInScope (PString "args") (PList evaledArgs) scope)

eval (ExpressionLiteral expression) scope _ =
    PRoutine $ PFunction (
        \newScope value ->
            eval expression newScope value
    )

eval (Expression nodes) scope value =
    fst $ foldl (\(val, scope) ast -> processValue (eval ast scope val, scope)) (value, scope) nodes

eval (Lookup name) scope _ = findFromScope (PString name) scope
eval (NumLiteral num) _ _ = PNum num
eval (StringLiteral s) _ _ = PString s

