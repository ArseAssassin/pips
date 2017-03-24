module Terp (runScript) where

import AST
import Runtime
import StdLib

runScript :: ASTNode -> PValue
runScript node =
    eval node defaultScope (PScope defaultScope)

processValue ((PAssignScope newScope), _) = (PScope newScope, newScope)
processValue ((PRoutine (PGetScope fn)), scope) = processValue ((fn scope), scope)
processValue it = it

eval :: ASTNode -> Scope -> PValue -> PValue
eval (Term (fn:args)) scope value =
        case eval fn scope value of
            PRoutine (PExpression fn') ->
                fn' (map (\astNode ->
                    eval astNode scope value
                ) args) value
            PError it -> PError it
            it -> PError ("Calling invalid function" ++ (show it))


eval (ExpressionLiteral expression) scope _ =
    PRoutine $ PExpression (
        \_ value ->
            eval expression scope value
    )

eval (Expression nodes) scope value =
    fst $ foldl (\it@(val, scope) ast -> processValue (eval ast scope val, scope)) (value, scope) nodes

eval (Lookup name) scope _ = findFromScope (PString name) scope
eval (NumLiteral num) _ _ = PNum num
eval (StringLiteral s) _ _ = PString s

