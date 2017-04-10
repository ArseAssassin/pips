{-# LANGUAGE FlexibleInstances #-}
module Runtime where

import Data.List (intercalate)
import qualified Data.Map.Lazy as Map

type Object = Map.Map PValue PValue
data Scope = Scope Object Object deriving (Eq, Ord, Show)
type Function = Scope -> [PValue] -> PValue -> PValue

showObject :: Object -> String
showObject it = showPairs $ Map.toList it

showPairs it = "(hashmap " ++ s ++ ")"
    where s = unwords $ map (
                \(key, value) ->
                    (show key) ++ " " ++ (show value)
            ) it

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

instance Ord PValue where
    compare (PSymbol "_") _ = EQ
    compare _ (PSymbol "_") = EQ
    compare (PNum a) (PNum b) = compare a b
    compare (PString a) (PString b) = compare a b
    compare (PSymbol a) (PSymbol b) = compare a b
    compare (PThrown a) (PThrown b) = compare a b
    compare (PScope a) (PScope b) = compare a b
    compare (PHashMap a) (PHashMap b) = compare a b
    compare (PAssignScope a) (PAssignScope b) = compare a b
    compare (PMeta _ _ a) b = compare a b
    compare b (PMeta _ _ a) = compare a b
    compare (PList a) (PList b) = compare a b
    compare (PBool a) (PBool b) = compare a b
    compare (PError a _) (PError b _) = compare a b
    compare (PFunction _) _ = EQ
    compare _ (PFunction _) = EQ
    compare (PInterrupt _) _ = EQ
    compare _ (PInterrupt _) = EQ
    compare _ _ = EQ

instance Show PValue where
    show (PNum it) = (show it)
    show (PString it) = it
    show (PError value it) = "(Error " ++ (show value) ++ " " ++ it ++ ")"
    show (PThrown it) = "(UncaughtError " ++ (show it) ++ ")"
    show (PScope (Scope locals _)) = "(Scope " ++ (showObject locals) ++ ")"
    show (PAssignScope (Scope locals _)) = "(AssignScope " ++ (showObject locals) ++ ")"
    show (PMeta _ _ value@(PMeta _ _ _)) = show value
    show (PMeta _ _ value) = "~" ++ show value
    show (PHashMap it) = "(hashmap " ++ (show it) ++ ")"
    show (PFunction _) = "(Function)"
    show (PList []) = "(list)"
    show (PList values) = "(list " ++ (unwords $ map show values) ++ ")"
    show (PBool it) = (show it)
    show (PInterrupt _) = "(Interrupt)"
    show (PSymbol it) = "(symbol " ++ it ++ ")"

putInScope :: PValue -> PValue -> Scope -> Scope
putInScope name value (Scope local parent) =
    Scope (Map.insert name value local) parent

putInLib name value (Scope local parent) =
    Scope local (Map.insert name value parent)

findValue :: PValue -> Scope -> Maybe PValue
findValue (PString "scope") it = Just $ PScope it

findValue name (Scope local parent) =
    case (Map.!?) local name of
        Nothing -> (Map.!?) parent name
        it -> it

findFromScope name scope =
    findValue name scope

mergeScopes :: Scope -> Scope -> Scope
mergeScopes (Scope locals lib) (Scope locals' lib') =
    (Scope (mergeObjects locals locals') (mergeObjects lib lib'))
    where mergeObjects a b = Map.fromList $ (Map.toList b) ++ (Map.toList a)

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty
