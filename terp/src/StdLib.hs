module StdLib where

import Runtime
import Data.List.Split (splitOn)
import Data.List (find)

argError _ _ _ it@(PError _ _) = it
argError name expected args value = PError (PString "ArgumentError") $ "Invalid arguments: " ++ name ++ " expects " ++ expected ++ " as argument but received " ++ (show value) ++ " as value, " ++ (show args) ++ " as args"

unmetaArgs fn scope args val = fn scope (map unmeta args) val
passArgErrors fn scope args val =
    case find (\it -> case it of
            PError _ _ -> True
            _ -> False
        ) args of
        Just it -> it
        _ -> fn scope args val
normalizeArgs fn = unmetaArgs $ passArgErrors fn

unmetaValue fn scope args val = fn scope args $ unmeta val

passError _ _ _ it@(PError _ _) = it
passError fn scope args val = fn scope args val

normalizeValue fn = unmetaValue $ passError fn

normalizeAll fn = normalizeArgs $ normalizeValue fn

adjustFn fn (name, value) = (name, fn value)

plus _ [(PNum b)] (PNum a) =  PNum $ a + b
plus _ args val = argError "+" "Number" args val

minus _ [(PNum b)] (PNum a) =  PNum $ a - b
minus _ args val = argError "-" "Number" args val

multiply _ [(PNum b)] (PNum a) = PNum $ a * b

mod' _ (PNum b:[]) (PNum a) = PNum $ mod a b
mod' _ args val = argError "mod" "Num, Num" args val

eq _ (arg:[]) val = PBool $ unmeta arg == unmeta val

range _ ((PNum max):[]) (PNum i) =
    PList $ map PNum [i..max]

join :: Function
join scope args@(PString glue:[]) (PList ((PString a):(PString b):xs)) =
    join scope args (PList ((PString (a ++ glue ++ b)) : xs))
join _ _ (PList [it]) = it
join _ args val = argError "join" "List, String" args val


str scope values _ = join scope [PString " "] (PList (map (PString . show) values))

flatten' :: Function
flatten' scope [] (PList ((PList a):(PList b):xs)) =
    flatten' scope [] $ PList $ (a ++ b) ++ xs
flatten' scope [] (PList (a:(PList b):xs)) =
    flatten' scope [] $ PList $ (a : b) ++ xs
flatten' _ [] (PList [it]) = it
flatten' _ [] it@(PList _) = it
flatten' _ args value = argError "flatten" "[[Any]]" args value

not' _ [] (PBool a) = PBool $ not a

lt _ (PNum b:[]) (PNum a) = PBool $ a < b
lt _ args val = argError "<" "Num, Num" args val

gt _ (PNum b:[]) (PNum a) = PBool $ a > b

head' _ [] (PList (a:_)) = a
head' _ args val = argError "head" "List" args val

tail' _ [] (PList []) = PError (PString "ArgumentError") "Calling tail on an empty list"
tail' _ [] (PList a) = PList $ tail a
tail' _ args value = argError "tail" "list, list" args value

prepend _ (value:[]) (PList a) = PList $ value : a

getElement _ [PNum i] (PList it) =
    if length it <= i
        then PError (PString "OutOfBoundsError") $ "List element " ++ (show i) ++ " out of bounds on list " ++ (show it)
        else it !! i

last' _ [] (PList it) = last it

-- scope' [] val = PRoutine $ PGetScope
--     (\scope -> PAssignScope $ putInScope (PString "it") val scope)

zip' _ [(PList b)] (PList a) = PList $ map (\(a, b) -> PList [a, b]) $ zip a b
zip' _ args val = argError "zip" "list, list" args val

quoteNames = map $ \(name, value) -> (PString name, value)

split _ (PString "":[]) (PString s) =
    PList $ map PString $ drop 1 $ splitOn "" s

split _ (PString separator:[]) (PString s) =
    PList $ map PString $ splitOn separator s

putName name value = PMeta (PString "name") name value

resolvingArgs =
    map (\(name, fn) -> (name, putName name $ PFunction fn))

len _ [] (PList it) = PNum $ length it
len _ args value = argError "len" "[Any]" args value

concat' _ [(PList b)] (PList a) = PList $ a ++ b
concat' _ args value = argError "concat" "List, List" args value

defaultExpressions :: Object
defaultExpressions =
    resolvingArgs
    $ map (adjustFn normalizeAll)
    $ quoteNames [
        ("+", plus),
        ("*", multiply),
        ("-", minus),
        ("<", lt),
        (">", gt),
        ("zip", zip'),
        ("concat", concat'),
        ("split", split),
        (".", getElement),
        ("prepend", prepend),
        ("head", head'),
        ("tail", tail'),
        ("last", last'),
        ("len", len),
        ("join", join),
        ("str", str),
        ("==", eq),
        ("not", not'),
        ("mod", mod'),
        ("range", range),
        ("flatten", flatten')
    ]


apply scope ((PFunction fn):(PList args):[]) value =
    fn scope args value

apply _ args value = argError "apply" "Any, Any -> Any, List" args value

-- adjust (PNum i:PFunction fn:[]) scope (PList it) =
--     PList (take i it ++ (fn (addArgs [] scope) (it !! i)) : drop (i + 1) it)

-- curry' args scope (PFunction fn) =
--     PFunction (\newScope newValue -> fn (addArgs (args ++ (
--         case findFromScope (PString "args") newScope of
--             Just (PList it) -> it
--             _ -> []
--     )) scope) newValue)

-- foldl' (startValue:PFunction fn:[]) scope (PList list) =
--     foldl (\memo next -> fn (addArgs [next] scope) memo) startValue list

-- map' (PFunction fn:[]) scope (PList values) =
--         PList $ map (fn (addArgs [] scope)) values
-- map' args _ val = argError "map" "[Any], Any -> Any" args val


-- filter' routine@(PFunction fn:[]) scope (PList (val:rest)) =
--     case fn scope val of
--         PBool isValid ->
--             case filter' routine scope (PList rest) of
--                 PList filtered -> PList (if isValid then (val : filtered) else filtered)

--         it@(PError _ _) -> it
--         it -> it

-- filter' _ _ (PList []) = PList []

-- filter' args _ val = argError "filter" "any -> Boolean, List" args val

-- pipe :: [PValue] -> Scope -> PValue -> PValue
-- pipe [_] scope value = value
-- pipe ((PNum i):(PFunction fn):xs) scope value =
--     pipe ((PNum i) : (drop i xs)) scope (fn (addArgs args scope) value)
--     where args = take i xs

-- pipe args _ val = argError "pipe" "Any, [Fn, Any]" args val


scopeExpressions =
    resolvingArgs
    $ quoteNames [
--         (">>", \args scope value -> pipe ((PNum 1) : args) scope value),
--         (">>>", \args scope value -> pipe ((PNum 2) : args) scope value),
--         (">n", pipe),
--         ("adjust", adjust),
--         ("foldl", foldl'),
--         ("map", map'),
        -- ("filter", filter'),
        ("apply", unmetaArgs apply)
--         ("curry", curry')
    ]

meta :: Scope -> [PValue] -> PValue -> PValue
meta _ (name:metaValue:[]) value = PMeta name metaValue value
meta scope (name:[]) (PMeta metaName value child) =
    if metaName == name then value else meta scope [name] child
meta _ (name:[]) value = PError (PString "LookupError") $ "Metadata " ++ (show name) ++ " not found from " ++ (show value)
meta _ [] value =
    PHashMap $ reverse $ meta' value
    where meta' (PMeta metaName metaValue value) = (metaName, metaValue) : meta' value
          meta' value = []

unmeta (PMeta _ _ it) = unmeta it
unmeta any = any
unmeta' _ [] it = unmeta it

doc scope (doc:[]) value = meta scope [PString "doc", doc] value
doc scope [] value =
    case meta scope [PString "doc"] value of
        PError error _ -> PError error $ "No documentation attached to " ++ (show $ unmeta value)
        it -> it

hashMap' [] = []
hashMap' (value:name:xs) = (value, name) : hashMap' xs
hashMap _ args _ = PHashMap $ hashMap' args

metaExpressions =
    resolvingArgs
    $ quoteNames [
        ("meta", passError $ passArgErrors meta),
        ("import", passError import'),
        ("unmeta", passError unmeta'),
        ("hashmap", passError hashMap),
        ("doc", passError $ passArgErrors doc),
        ("scope", passError $ \scope _ _ -> PScope scope)
    ]


put' (name:value:xs) = (name, PMeta (PString "name") name value) : (put' xs)
put' [] = []

put _ args (PScope scope) =
    if (mod (length args) 2) > 0 then
        PError (PString "ArgumentError") $ "= expects even number of arguments, received " ++ (show args)
    else PScope $ (put' args) ++ scope

put _ args value = argError "put" "Scope, [[Any, Any]]" args value

assign scope args _ =
    if (mod (length args) 2) > 0 then
        PError (PString "ArgumentError") $ "= expects even number of arguments, received " ++ (show args)
    else PAssignScope $ put' args


catch scope (caughtType:value:[]) error@(PError typeName _) =
    if typeName == caughtType then value else error

catch scope (_:_:[]) value =
    value

catch _ args value =
    argError "catch" "Any, String, Function" args value

isError scope args (PMeta _ _ value) =
    isError scope args value

isError _ (name:[]) it@(PError typeName _) =
    if typeName == name then PBool True else PBool False

isError _ _ _ = PBool False

-- log' _ [name] value =
--     PInput $ do
--         putStrLn $ "Log (" ++ (show name) ++ "): " ++ (show value)
--         return value

-- log' _ [] value = PInput $ do
--     putStrLn $ "Log: " ++ (show value)
--     return value
error' _ (PString t:PString msg:[]) value =
    PError (PString t) msg
error' _ args value =
    argError "error" "Void, String, String" args value

and' scope args (PMeta _ _ value) = and' scope args value
and' _ [_] (PBool False) = (PBool False)
and' _ [expression] (PBool True) = expression
and' _ [_] it = it
and' _ args value = argError "and" "Any, Any" args value

or' scope args (PMeta _ _ value) = or' scope args value
or' _ [expression] (PBool False) = expression
or' _ [_] value = value
or' _ args value = argError "or" "Any, Any" args value

import' _ _ (PScope scope) = PAssignScope scope
import' _ args value = argError "import" "Scope" args value

to _ _ it@(PError _ _) = it
to _ [value] _ = value
to _ args val = argError "to" "Any, Any" args val

toUnsafe _ [value] _ = value
toUnsafe _ args val = argError "to" "Any, Any" args val

if' scope ((PMeta _ _ value):rest) val =
    if' scope (value : rest) val

if' _ ((PBool True):value:_) _ =
    value

if' scope ((PBool False):_:rest) value =
    if' scope rest value

if' _ [else'] _ =
    else'

if' scope args value = argError "if" "Any, [[Boolean, Any], Any]" args value

bareExpressions =
    map (\(name, value) -> (name, PMeta (PString "name") name value))
    $ quoteNames [
        ("=", PFunction assign),
        ("put", PFunction $ passError put),
        ("catch", PFunction $ unmetaArgs $ unmetaValue catch),
        ("isError", PFunction $ unmetaArgs $ unmetaValue isError),
        ("error", PFunction error'),
        ("comment", PFunction $ \_ _ value -> value),
        -- ("log", PFunction log'),
        ("newScope", PScope []),
        ("and", PFunction $ unmetaValue $ passError and'),
        ("or", PFunction $ unmetaValue $ passError or'),
        ("if", PFunction if'),
        ("to", PFunction $ passError to),
        ("!to", PFunction to),
        ("log", PFunction (\_ args val -> PInterrupt (
            do putStrLn $ "Log(" ++ (show args) ++ "): " ++ (show val)
               return val,
            id
        )))
    ]


defaultValues :: Object
defaultValues = quoteNames [("True", PBool True), ("False", PBool False)]

defaultLib :: Object
defaultLib = concat [defaultExpressions, defaultValues, metaExpressions, bareExpressions, scopeExpressions]

defaultScope :: Scope
defaultScope = defaultLib
