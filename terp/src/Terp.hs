module Terp (runScript) where

import Data.Function ((&))
import qualified Data.Map.Lazy as Map
import Data.List (find, intercalate)

import Text.ParserCombinators.Parsec.Pos (sourceName, sourceLine, sourceColumn, SourcePos)
import Data.List.Split (splitOn)
import AST
import Runtime
import StdLib
import StdLib.Helpers (unmeta)
import PIPsParser

type Interrupt = (IO PValue, PValue -> PValue)

compilePath :: Object -> String -> Maybe String
compilePath libraryPath name =
    case libPath of
        Just (PString it) -> Just $ (intercalate "/" (it:rest)) ++ ".pip"
        _ -> Nothing
    where
        (pathId:rest) = splitOn "/" name
        libPath = (Map.!?) libraryPath (PString pathId)

runScript :: ASTNode -> Scope -> PValue -> IO PValue
runScript it scope value =
    handleInterrupts $ eval it updatedScope value
    where updatedScope = putInLib (PString "require") (PFunction $ require' scope) scope

handleInterrupts value =
    case value of
        PInterrupt (io, fn) -> do
            value <- io
            handleInterrupts $ fn value
        it -> return it

require' scope _ [PString name] _ =
    PInterrupt (
        do
            case findFromScope (PString "libraryPath") scope of
                Just (PHashMap path) ->
                    case compilePath path name of
                        Just fileName -> do
                            script <- readFile fileName
                            case parsePIPs name script of
                                Left it -> return $ PThrown $ PError (PString "ParsingError") (show it)
                                Right it -> runScript it defaultScope (PScope defaultScope)
                        _ -> return $ PThrown $ PError (PString "ImportError") "Couldn't find named library from path"

                it -> do
                    putStrLn (show scope)
                    return $ PThrown $ PError (PString "ImportError") $ "Library path corrupted - expected a hash map, found instead: " ++ (show it),

        id
    )


addSourcePos :: PValue -> SourcePos -> PValue
addSourcePos (PThrown (PError typeName it)) sourcePos =
    PThrown $ PError typeName $
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
        it@(PThrown _) -> it
        _ -> evalExpression (s, v) rest

evalExpression (scope, value) [] = value

eval :: ASTNode -> Scope -> PValue -> PValue
eval (Expression nodes sourcePos) scope value =
    evalExpression (scope, value) nodes

eval (Term (fn:args) sourcePos) scope value =
        let evaledFn = eval fn scope value
            evaledArgs = map (\it -> eval it scope value) args
            updatedScope = case meta scope [(PString "accessParentScope")] evaledFn of
                                PHashMap vals -> Scope (Map.fromList $ map (\(name, parentName) ->
                                                        case findFromScope parentName scope of
                                                            Just it -> (name, it)
                                                            Nothing -> (name, PThrown $ PError (PString "LookupError") $ "Couldn't find value " ++ (show name) ++ " from parent scope")
                                                       ) $ Map.toList vals)
                                                       Map.empty
                                _ -> emptyScope
                            & (putInLib (PString "__functionMeta") (meta scope [] evaledFn))

        in case unmeta evaledFn of
            PFunction fn ->
                case fn updatedScope evaledArgs value of
                    PThrown (PError typeName it) ->
                        case meta scope [PString "name"] evaledFn of
                            PThrown (PError _ _) -> PThrown $ PError typeName $ it ++ "\n" ++ (show sourcePos) ++ ": anonymous"
                            PString name -> PThrown $ PError typeName $ it ++ "\n" ++ (show sourcePos) ++ ": " ++ name
                    it -> it
            PThrown (PError typeName it) ->
                addSourcePos (
                    case meta updatedScope [PString "name"] evaledFn of
                        PThrown (PError (PString "LookupError") _) -> PThrown $ PError typeName it
                        name -> PThrown $ PError typeName $ "While calling function " ++ (show name) ++ ": " ++ it
                ) sourcePos
            it -> addSourcePos (PThrown (PError (PString "ValueError") $ "Calling invalid function " ++ (show it))) sourcePos

eval (ExpressionLiteral astNode) scope _ =
    fn
    where fn = PFunction $
                \newScope args value ->
                    let metaFn = case findFromScope (PString "__functionMeta") newScope of
                                    Just (PHashMap it) -> foldl (\value (name, val) -> PMeta name val value) fn $Map.toList it
                                    Nothing -> fn
                        newLocals = [(PString "_args", (PList args)),
                                     (PString "_input", value),
                                     (PString "_recur", fn)]
                        updatedScope = mergeScopes (Scope Map.empty (Map.fromList newLocals))
                                       $ mergeScopes newScope scope
                        in eval astNode updatedScope value

eval (Lookup name) scope _ =
    case findFromScope (PString name) scope of
        Just it -> it
        Nothing -> PThrown $ PError (PString "LookupError") $ "Couldn't find value " ++ (show name) ++ " from scope"

eval (NumLiteral i) _ _ = PNum i
eval (StringLiteral s) _ _ = PString s

