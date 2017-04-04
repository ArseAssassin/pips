module Terp (runScript) where

import Data.Function ((&))

import Text.ParserCombinators.Parsec.Pos (sourceName, sourceLine, sourceColumn, SourcePos)
import AST
import Runtime
import StdLib
import PIPsParser

type Interrupt = (IO PValue, PValue -> PValue)

runScript :: ASTNode -> Scope -> IO PValue
runScript it scope =
    handleInterrupts $ eval it updatedScope (PScope updatedScope)
    where updatedScope = putInScope (PString "require") (PFunction require') scope

handleInterrupts value =
    case value of
        PInterrupt (io, fn) -> do
            value <- io
            handleInterrupts $ fn value
        it -> return it

require' _ [PString name] _ =
    PInterrupt (
        do
            script <- readFile name
            case parsePIPs name script of
                Left it -> return $ PError (PString "ParsingError") (show it)
                Right it -> runScript it defaultScope,
        id
    )


addSourcePos :: PValue -> SourcePos -> PValue
addSourcePos (PError typeName it) sourcePos =
    PError typeName $
        it ++
        "\n  in " ++
        (sourceName sourcePos) ++
        ":" ++ (show (sourceLine sourcePos)) ++
        ":" ++ (show (sourceColumn sourcePos))


evalExpression (scope, val) (astNode:rest) =
    let (s, v) = case val of
                    newVal@(PAssignScope it) -> (s, eval astNode s newVal)
                        where s = mergeScopes it scope
                    _ -> (scope, eval astNode scope val)
    in case v of
        PInterrupt (io, fn) -> PInterrupt (io, \value -> evalExpression (s, fn value) rest)
        _ -> evalExpression (s, v) rest

evalExpression (scope, value) [] = value

eval :: ASTNode -> Scope -> PValue -> PValue
eval (Expression nodes sourcePos) scope value =
    evalExpression (scope, value) nodes

eval (Term (fn:args) sourcePos) scope value =
        let evaledFn = eval fn scope value
            evaledArgs = map (\it -> eval it scope value) args
            updatedScope = case meta scope [(PString "accessParentScope")] evaledFn of
                                PHashMap vals -> map (\(name, parentName) ->
                                                    case findFromScope parentName scope of
                                                        Just it -> (name, it)
                                                        Nothing -> (name, PError (PString "LookupError") $ "Couldn't find value " ++ (show name) ++ " from parent scope")
                                                ) vals
                                _ -> scope
                            & (putInScope (PString "__functionMeta") (meta scope [] evaledFn))
                            & \scope -> case meta scope [(PString "includeLocals")] evaledFn of
                                        PHashMap vals -> mergeScopes scope vals
                                        _ -> scope
        in case unmeta evaledFn of
            PFunction fn ->
                case fn updatedScope evaledArgs value of
                    PError typeName it ->
                        case meta scope [PString "name"] evaledFn of
                            PError _ _ -> PError typeName $ it ++ "\n" ++ (show sourcePos) ++ ": anonymous"
                            PString name -> PError typeName $ it ++ "\n" ++ (show sourcePos) ++ ": " ++ name
                    it -> it
            PError typeName it ->
                addSourcePos (
                    case meta updatedScope [PString "name"] evaledFn of
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

