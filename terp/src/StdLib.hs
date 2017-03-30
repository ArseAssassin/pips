module StdLib where

import Runtime
import Data.List.Split (splitOn)

argError _ _ _ it@(PError _ _) = it
argError name expected args value = PError (PString "ArgumentError") $ "Invalid arguments: " ++ name ++ " expects " ++ expected ++ " as argument but received " ++ (show value) ++ " as value, " ++ (show args) ++ " as args"

plus [(PNum b)] (PNum a) =  PNum $ a + b
plus args val = argError "+" "Number" args val

minus [(PNum b)] (PNum a) =  PNum $ a - b
minus args val = argError "-" "Number" args val

multiply [(PNum b)] (PNum a) = PNum $ a * b


mod' (PNum b:[]) (PNum a) = PNum $ mod a b
mod' args val = argError "mod" "Num, Num" args val

eq (arg:[]) val = PBool $ arg == val

range ((PNum max):[]) (PNum i) =
    PList $ map PNum [i..max]

addArgs args scope = putInScope (PString "args") (PList args) scope

join args@(PString glue:[]) (PList ((PString a):(PString b):xs)) =
    join args (PList ((PString (a ++ glue ++ b)) : xs))
join _ (PList [it]) = it
join args val = argError "join" "List, String" args val


str [] val = PString (show val)

flatten' [] (PList ((PList a):(PList b):xs)) =
    flatten' [] $ PList $ (a ++ b) ++ xs
flatten' [] (PList [it]) = it
flatten' [] it@(PList _) = it
flatten' args value = argError "flatten" "[[Any]]" args value

-- fn names (PScope scope) =
--     case last names of
--         (PRoutine (PExpression fn)) ->
--             PRoutine $ PExpression (\args _ ->
--                 PAssignScope scope
--             )

not' [] (PBool a) = PBool $ not a

identity _ value = value

lt (PNum b:[]) (PNum a) = PBool $ a < b
lt args val = argError "<" "Num, Num" args val

gt (PNum b:[]) (PNum a) = PBool $ a > b

head' [] (PList (a:_)) = a
head' args val = argError "head" "List" args val

tail' [] (PList a) = PList $ tail a
tail' args value = argError "tail" "list, list" args value

prepend (value:[]) (PList a) = PList $ value : a

getElement [PNum i] (PList it) = it !! i

list args _ = PList args

take' (PNum i:[]) (PList it) = PList $ take i it
take' args value = argError "take" "[Any], Num" args value

drop' (PNum i:[]) (PList it) = PList $ drop i it
last' [] (PList it) = last it

-- scope' [] val = PRoutine $ PGetScope
--     (\scope -> PAssignScope $ putInScope (PString "it") val scope)

zip' [(PList b)] (PList a) = PList $ map (\(a, b) -> PList [a, b]) $ zip a b
zip' args val = argError "zip" "list, list" args val

quoteNames = map $ \(name, value) -> (PString name, value)

split (PString "":[]) (PString s) =
    PList $ map PString $ drop 1 $ splitOn "" s

split (PString separator:[]) (PString s) =
    PList $ map PString $ splitOn separator s

putName name value = PMeta (PString "name") name value

resolvingArgs adapt =
    map (\(name, fn) -> (name, putName name $ PFunction (
        \scope args value ->
            return $ case unmeta value of
                PError errorType it -> PError errorType it
                _ -> adapt fn args scope value
    )))

len [] (PList it) = PNum $ length it
len args value = argError "len" "[Any]" args value

defaultExpressions :: Object
defaultExpressions =
    resolvingArgs (\fn args _ value -> fn (map unmeta args) (unmeta value))
    $ quoteNames [
        ("+", plus),
        ("*", multiply),
        ("<", lt),
        (">", gt),
        ("zip", zip'),
        ("split", split),
        (".", getElement),
        ("prepend", prepend),
        ("head", head'),
        ("tail", tail'),
        ("take", take'),
        ("drop", drop'),
        ("last", last'),
        ("len", len),
        ("comment", identity),
        ("join", join),
        ("str", str),
        ("==", eq),
        ("!=", \args value -> not' [] (eq args value)),
        ("!", not'),
        ("mod", mod'),
        ("range", range),
        ("-", minus),
        ("flatten", flatten'),
        ("list", list)
    ]

apply scope ((PFunction fn):(PList args):[]) value =
    fn scope args value

apply _ args value = return $ argError "apply" "Any -> Any, List, Any" args value

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
    map (\(name, fn) -> (name, putName name $ PFunction (\scope args value ->
        fn scope (map unmeta args) value)))
    $ quoteNames [
--         (">>", \args scope value -> pipe ((PNum 1) : args) scope value),
--         (">>>", \args scope value -> pipe ((PNum 2) : args) scope value),
--         (">n", pipe),
--         ("adjust", adjust),
--         ("foldl", foldl'),
--         ("map", map'),
        -- ("filter", filter'),
        ("apply", apply)
--         ("curry", curry')
    ]

meta :: [PValue] -> Scope -> PValue -> PValue
meta (name:metaValue:[]) _ value = PMeta name metaValue value
meta (name:[]) scope (PMeta metaName value child) =
    if metaName == name then value else meta [name] scope child
meta (name:[]) _ value = PError (PString "LookupError") $ "Metadata " ++ (show name) ++ " not found from " ++ (show value)

unmeta (PMeta _ _ it) = unmeta it
unmeta any = any
unmeta' [] _ it = unmeta it

doc (doc:[]) scope value = meta [PString "doc", doc] scope value
doc [] scope value =
    case meta [PString "doc"] scope value of
        PError error _ -> PError error $ "No documentation attached to " ++ (show $ unmeta value)
        it -> it

put' (name:value:xs) = (name, PMeta (PString "name") name value) : (put' xs)
put' [] = []

put args scope _ =
    if (mod (length args) 2) > 0 then
        PError (PString "ArgumentError") $ "= expects even number of arguments, received " ++ (show args)
    else PAssignScope $ put' args

to [value] _ _ = value
to args _ val = argError "to" "Any, Any" args val

and' [_] _ (PBool False) = (PBool False)
and' [expression] _ (PBool True) = expression
and' _ _ it = it

or' [expression] _ (PBool False) = expression
or' _ _ value = value


metaExpressions =
    resolvingArgs (\fn args scope value -> fn args scope value)
    $ quoteNames [
        ("meta", meta),
        ("=", put),
        ("unmeta", unmeta'),
        ("doc", doc),
        ("scope", \_ scope _ -> PScope scope),
        ("and", and'),
        ("or", or'),
        ("to", to)
    ]



catch :: Scope -> [PValue] -> PValue -> IO PValue
catch _ (name:value:[]) it@(PError typeName _) =
    return $ if typeName == name then value else it

catch _ _ any = return $ any

bareExpressions =
    quoteNames [
        ("catch", PFunction catch)
    ]


defaultValues :: Object
defaultValues = quoteNames [("True", PBool True), ("False", PBool False)]

defaultLib :: Object
defaultLib = concat [defaultExpressions, defaultValues, metaExpressions, bareExpressions, scopeExpressions]

defaultScope :: Scope
defaultScope = defaultLib
