{-# LANGUAGE FlexibleInstances #-}
module Runtime where

import Data.List (find)

type Object = [(PValue, PValue)]
type Scope = Object
type Function = Scope -> [PValue] -> PValue -> IO PValue

instance Eq Function where
    a == b = False

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
    show (PError value it) = "Error " ++ (show value) ++ " " ++ it
    show (PScope it) = show it
    show (PAssignScope it) = "AssignScope " ++ (show it)
    show (PMeta _ _ value@(PMeta _ _ _)) = show value
    show (PMeta _ _ value) = "(meta " ++ show value ++ ")"
    show (PFunction _) = "Function"
    show (PList values) = "(list " ++ (unwords $ map show values) ++ ")"
    show (PBool it) = (show it)


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
