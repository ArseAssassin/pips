module Runtime where

import Data.List (find)

type PArgs = [PValue]
type Expression = (PArgs -> PValue -> PValue)

data PExpression =
    PExpression Expression

instance Eq PExpression where
    _ == _ = False

instance Show PExpression where
    show _ = "PExpression"

data PValue =
    PNum Int |
    PString String |
    PError String |
    PObject Object |
    PRoutine PExpression |
    PAssignScope Object Bool PExpression |
    PScope Scope |
    PList [PValue] |
    PBool Bool deriving (Show, Eq)

type Object = [(PValue, PValue)]

data Scope = NoScope | Scope Object Scope deriving (Show, Eq)

findValue :: PValue -> Object -> PValue
findValue name object =
    case find ((name == ) . fst) object of
        Nothing -> PError ("Couldn't find key ")
        Just (_, x) -> x

findFromScope :: PValue -> Scope -> PValue
findFromScope name NoScope = PError $ "Tried looking up non-existent value from scope " ++ (show name)
findFromScope name (Scope content parent) =
    case findValue name content of
        PError _ -> findFromScope name parent
        x -> x

putInScope :: PValue -> PValue -> Scope -> Scope
putInScope name value (Scope content parent) =
    Scope ((name, value) : content) parent
