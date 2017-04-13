module StdLib.Helpers where

import Runtime
import Data.List (find)

quoteNames = map $ \(name, value) -> (PString name, value)

argError _ _ _ it@(PThrown _) = it
argError name expected args value = PThrown $ PError (PString "ArgumentError") $ "Invalid arguments: " ++ name ++ " expects " ++ expected ++ " as argument but received \n" ++ (show value) ++ " as value, \n" ++ (show args) ++ " as args"

unmetaArgs fn scope args val = fn scope (map unmeta args) val
passArgErrors fn scope args val =
    case find (\it -> case it of
            PThrown _ -> True
            _ -> False
        ) args of
        Just it -> it
        _ -> fn scope args val
normalizeArgs fn = unmetaArgs $ passArgErrors fn

unmetaValue fn scope args val = fn scope args $ unmeta val

normalizeValue fn = unmetaValue fn

normalizeAll fn = normalizeArgs $ normalizeValue fn

adjustFn fn (name, value) = (name, fn value)

unmeta (PMeta _ _ it) = unmeta it
unmeta any = any
