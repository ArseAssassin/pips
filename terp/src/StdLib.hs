module StdLib where

import Runtime
import Data.List (intercalate)

argError _ _ _ (PError error) = (PError error)
argError name expected args value = PError $ "Invalid arguments: " ++ name ++ " expects " ++ expected ++ " as argument but received " ++ (show value) ++ ", " ++ (show args)

plus [(PNum b)] (PNum a) =  PNum $ a + b
plus args val = argError "+" "Number" args val

minus [(PNum b)] (PNum a) =  PNum $ a - b
minus args val = argError "-" "Number" args val

multiply [(PNum b)] (PNum a) = PNum $ a * b

to [value] _ = value
to args val = argError "to" "[]" args val

put (name:value:xs) (PScope scope) =
    put xs $ PScope (putInScope name value scope)

put [] it@(PScope scope) = PAssignScope scope
put args val = argError "=" "String, Function" args val

and' [_] (PBool False) = (PBool False)
and' [expression] (PBool True) = expression
and' _ it = it

or' [expression] (PBool False) = expression
or' _ value = value

pipe [_] value = value
pipe ((PNum i):(PRoutine (PExpression fn)):xs) value =
    pipe ((PNum i) : (drop i xs)) (fn args value)
    where args = (take i xs)

pipe args val = argError "pipe" "[any, num:args]" args val

connectPipe _ next = next

mod' (PNum b:[]) (PNum a) = PNum $ mod a b
mod' args val = argError "mod" "Num, Num" args val

eq (arg:[]) val = PBool $ arg == val

range ((PNum max):[]) (PNum i) =
    PList $ map PNum [i..max]

curry' (PRoutine (PExpression fn):args) _ =
    PRoutine $ PExpression (\newArgs value -> fn (args ++ newArgs) value)

foldl' (startValue:(PRoutine (PExpression fn)):[]) (PList list) =
    foldl (\memo next -> fn [next] memo) startValue list


map' ((PRoutine (PExpression fn)):[]) (PList values) =
        PList $ map (\val -> fn [] val) values
map' args val = argError "map" "[Any], Any -> Any" args val

filter' routine@(PRoutine (PExpression fn):[]) (PList (val:rest)) =
    case fn [] val of
        PBool isValid ->
            case filter' routine (PList rest) of
                PList filtered -> PList (if isValid then (val : filtered) else filtered)

        PError it -> PError it
        it -> it

filter' _ (PList []) = PList []

filter' args val = argError "filter" "any -> Boolean, List" args val

join args@(PString glue:[]) (PList ((PString a):(PString b):xs)) =
    join args (PList ((PString (a ++ glue ++ b)) : xs))
join _ (PList [it]) = it
join args val = argError "join" "List, String" args val

adjust (PNum i:PRoutine (PExpression fn):[]) (PList it) =
    PList (take i it ++ (fn [] (it !! i)) : drop (i + 1) it)

str [] val = PString (show val)

fn names (PScope scope) =
    case last names of
        (PRoutine (PExpression fn)) ->
            PRoutine $ PExpression (\args _ ->
                PAssignScope scope
            )

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

scope' [] val = PRoutine $ PGetScope (\scope -> PAssignScope $ putInScope (PString "it") val scope)

zip' [(PList b)] (PList a) = PList $ map (\(a, b) -> PList [a, b]) $ zip a b
zip' args val = argError "zip" "list, list" args val

quoteNames = map $ \(name, value) -> (PString name, value)

defaultExpressions =
    map (\(name, fn) -> (name, PRoutine $ PExpression fn)) $
    quoteNames [
        ("+", plus),
        ("*", multiply),
        ("<", lt),
        (">", gt),
        ("adjust", adjust),
        ("zip", zip'),
        (".", getElement),
        ("prepend", prepend),
        ("head", head'),
        ("tail", tail'),
        ("comment", identity),
        ("foldl", foldl'),
        ("join", join),
        ("str", str),
        (">>", \args value -> pipe ((PNum 1) : args) value),
        (">>>", \args value -> pipe ((PNum 2) : args) value),
        (">n", pipe),
        ("fn", fn),
        ("==", eq),
        ("!==", \args value -> not' [] (eq args value)),
        ("!", not'),
        ("scope", scope'),
        ("mod", mod'),
        ("range", range),
        ("curry", curry'),
        ("-", minus),
        ("to", to),
        ("and", and'),
        ("or", or'),
        ("=", put),
        ("list", list),
        ("map", map'),
        ("filter", filter')
    ]

defaultValues :: Object
defaultValues = quoteNames [("True", PBool True), ("False", PBool False)]

defaultLib :: Object
defaultLib = concat [defaultExpressions, defaultValues]

defaultScope :: Scope
defaultScope = Scope defaultLib NoScope
