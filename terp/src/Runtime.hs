{-# LANGUAGE FlexibleInstances #-}
module Runtime where

import Data.List (find)

data Scope = NoScope | Scope Object Scope | CompositeScope Scope Scope deriving (Eq, Show)
type Object = [(PValue, PValue)]
type Function = Scope -> PValue -> PValue

instance Eq Function where
    a == b = False

instance Show Function where
    show it = "Function"

data PValue =
    PNum Int |
    PString String |
    PError PValue String |
    PScope Scope |
    PAssignScope Scope |
    PMeta PValue PValue PValue |
    PFunction Function |
    PList [PValue] |
    PBool Bool deriving (Eq)

instance Show PValue where
    show (PNum it) = (show it)
    show (PString it) = show it
    show (PError value it) = "PError " ++ (show value) ++ " " ++ it
    show (PScope _) = "PScope"
    show (PAssignScope _) = "PAssignScope"
    show (PMeta _ _ value) = show value
    show (PFunction _) = "PFunction"
    show (PList values) = "(list " ++ (unwords $ map show values) ++ ")"
    show (PBool it) = (show it)


putInScope :: PValue -> PValue -> Scope -> Scope
putInScope name value parent =
    Scope [(name, value)] parent

findValue :: PValue -> Object -> Maybe PValue
findValue name object =
    case find ((name == ) . fst) object of
        Nothing -> Nothing
        Just (_, x) -> Just x

findFromScope name NoScope = Nothing
findFromScope name (Scope content parent) =
    case findValue name content of
        Nothing -> findFromScope name parent
        it -> it


findFromScope name (CompositeScope a b) =
    case findFromScope name a of
        Nothing -> findFromScope name b
        it -> it

mergeScopes a b = CompositeScope a b
