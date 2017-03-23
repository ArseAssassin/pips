module Terp (runScript) where

import AST
import Runtime
import StdLib

runScript :: ASTNode -> PValue
runScript node =
    eval node defaultScope (PScope defaultScope)

eval :: ASTNode -> Scope -> PValue -> PValue
eval (Term (fn:args)) scope value =
        case eval fn scope value of
            PRoutine (PExpression fn') ->
                fn' scope (map (\astNode ->
                    eval astNode scope value
                ) args) value
            PError it -> PError it
            it -> PError ("Calling invalid function" ++ (show it))


eval (ExpressionLiteral expression) scope _ =
    PRoutine $ PExpression (
        \_ _ value ->
            eval expression scope value
    )

eval (Expression nodes) scope value =
    fst $ foldl (\(val, scope) ast ->
        case eval ast scope val of
            PAssignScope newScope -> (PScope newScope, newScope)
            x -> (x, scope)
        ) (value, scope) nodes

eval (Lookup name) scope _ = findFromScope (PString name) scope
eval (NumLiteral num) _ _ = PNum num
eval (StringLiteral s) _ _ = PString s

