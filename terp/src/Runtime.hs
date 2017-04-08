{-# LANGUAGE FlexibleInstances #-}
module Runtime where

import Data.List (find)

type Object = [(PValue, PValue)]
type Scope = Object
type Function = Scope -> [PValue] -> PValue -> PValue

instance Eq Function where
    a == b = False

instance Eq (IO PValue) where
    a == b = False

instance Eq (PValue -> PValue) where
    a == b = False

data PValue =
    PNum Int |
    PString String |
    PSymbol String |
    PError PValue String |
    PThrown PValue |
    PScope Scope |
    PHashMap Object |
    PAssignScope Scope |
    PMeta PValue PValue PValue |
    PFunction Function |
    PInterrupt (IO PValue, PValue -> PValue) |
    PList [PValue] |
    PBool Bool

instance Eq PValue where
    (PSymbol "_") == _ = True
    _ == (PSymbol "_") = True
    (PNum a) == (PNum b) = a == b
    (PString a) == (PString b) = a == b
    (PSymbol a) == (PSymbol b) = a == b
    (PThrown a) == (PThrown b) = a == b
    (PScope a) == (PScope b) = a == b
    (PHashMap a) == (PHashMap b) = a == b
    (PAssignScope a) == (PAssignScope b) = a == b
    (PMeta _ _ a) == (PMeta _ _ b) = a == b
    (PList a) == (PList b) = a == b
    (PBool a) == (PBool b) = a == b
    (PError a a') == (PError b b') = and [a == b, a' == b']
    (PFunction _) == _ = False
    (PInterrupt _) == _ = False
    _ == _ = False

showScope scope =
    show (filter (\(_, val) ->
                    case val of
                        PScope it -> it /= scope
                        _ -> True
         ) scope)

instance Show PValue where
    show (PNum it) = (show it)
    show (PString it) = it
    show (PError value it) = "(Error " ++ (show value) ++ " " ++ it ++ ")"
    show (PThrown it) = "(UncaughtError " ++ (show it) ++ ")"
    show scope@(PScope it) = "(Scope " ++ (showScope it) ++ ")"
    show (PAssignScope it) = "(AssignScope " ++ (showScope it) ++ ")"
    show (PMeta _ _ value@(PMeta _ _ _)) = show value
    show (PMeta _ _ value) = "(meta " ++ show value ++ ")"
    show (PHashMap it) = "(hashmap " ++ (showScope it) ++ ")"
    show (PFunction _) = "(Function)"
    show (PList []) = "(list)"
    show (PList values) = "(list " ++ (unwords $ map show values) ++ ")"
    show (PBool it) = (show it)
    show (PInterrupt _) = "(Interrupt)"
    show (PSymbol it) = "(symbol " ++ it ++ ")"

putInScope :: PValue -> PValue -> Scope -> Scope
putInScope name value parent =
    (name, value) : parent

findValue :: PValue -> Object -> Maybe PValue
findValue name object =
    case find ((name == ) . fst) object of
        Nothing -> Nothing
        Just (_, x) -> Just x

findFromScope name scope =
    findValue name scope

mergeScopes a b = concat [a, b]
