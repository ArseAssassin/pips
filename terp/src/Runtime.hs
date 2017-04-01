{-# LANGUAGE FlexibleInstances #-}
module Runtime where

import Data.List (find)

type Object = [(PValue, PValue)]
type Scope = Object
type Function = Scope -> [PValue] -> PValue -> PValue
type Input = IO PValue

instance Eq Function where
    a == b = False

instance Eq Input where
    a == b = False

data PValue =
    PNum Int |
    PString String |
    PError PValue String |
    PScope Scope |
    PHashMap Object |
    PAssignScope Scope |
    PMeta PValue PValue PValue |
    PFunction Function |
    PInput Input |
    PList [PValue] |
    PBool Bool deriving (Eq)

showScope scope =
    show (filter (\(_, val) ->
                    case val of
                        PScope it -> it /= scope
                        _ -> True
         ) scope)

instance Show PValue where
    show (PNum it) = (show it)
    show (PString it) = show it
    show (PError value it) = "Error " ++ (show value) ++ " " ++ it
    show scope@(PScope it) = "Scope " ++ (showScope it)
    show (PAssignScope it) = "AssignScope " ++ (showScope it)
    show (PMeta _ _ value@(PMeta _ _ _)) = show value
    show (PMeta _ _ value) = "(meta " ++ show value ++ ")"
    show (PHashMap it) = "HashMap " ++ (showScope it)
    show (PFunction _) = "Function"
    show (PList []) = "(list)"
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
