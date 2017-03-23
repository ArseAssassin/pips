module StdLib where

import Runtime

argError _ _ _ (PError error) = (PError error)
argError name expected args value = PError $ "Invalid arguments: " ++ name ++ " expects " ++ expected ++ " as argument but received " ++ (show value) ++ ", " ++ (show args)

plus _ [(PNum b)] (PNum a) =  PNum $ a + b
plus _ args val = argError "+" "Number" args val

minus _ [(PNum b)] (PNum a) =  PNum $ a - b
minus _ args val = argError "-" "Number" args val

multiply _ [(PNum b)] (PNum a) = PNum $ a * b

to _ [value] _ = value
to _ args val = argError "to" "[]" args val

put _ (name:value:[]) (PScope scope) =
    PAssignScope newScope
    where newScope = putInScope name value scope

put _ [] val = argError "=" "String, Function" "[]" val
put _ args val = argError "=" "String, Function" args val

and' _ [_] (PBool False) = (PBool False)
and' _ [expression] (PBool True) = expression
and' _ _ it = it

or' _ [expression] (PBool False) = expression
or' _ _ value = value

pipe _ [_] value = value
pipe scope ((PNum i):(PRoutine (PExpression fn)):xs) value =
    pipe scope ((PNum i) : (drop i xs)) (fn scope args value)
    where args = (take i xs)

pipe _ args val = argError "pipe" "[any, num:args]" args val

connectPipe _ next = next

mod' _ (PNum b:[]) (PNum a) = PNum $ mod a b
mod' _ args val = argError "mod" "Num, Num" args val

eq _ (arg:[]) val = PBool $ arg == val

range _ ((PNum max):[]) (PNum i) =
    PList $ map PNum [i..max]

curry' _ (PRoutine (PExpression fn):args) _ =
    PRoutine $ PExpression (\scope newArgs value -> fn scope (args ++ newArgs) value)

foldl' scope (startValue:(PRoutine (PExpression fn)):[]) (PList list) =
    foldl (\memo next -> fn scope [next] memo) startValue list


map' scope ((PRoutine (PExpression fn)):[]) (PList values) =
        PList $ map (\val -> fn scope [] val) values
map' _ args val = argError "map" "[Any], Any -> Any" args val

filter' :: Expression
filter' scope routine@(PRoutine (PExpression fn):[]) (PList (val:rest)) =
    case fn scope [] val of
        PBool isValid ->
            case filter' scope routine (PList rest) of
                PList filtered -> PList (if isValid then (val : filtered) else filtered)

        PError it -> PError it
        it -> it

filter' _ _ (PList []) = PList []

filter' _ args val = argError "filter" "any -> Boolean, List" args val

fn _ names (PScope scope) =
    case last names of
        (PRoutine (PExpression fn)) ->
            PRoutine $ PExpression (\_ args _ ->
                PAssignScope scope
            )


not' _ [] (PBool a) = PBool $ not a

identity _ _ value = value

lt _ (PNum b:[]) (PNum a) = PBool $ a < b
lt _ args val = argError "<" "Num, Num" args val

gt _ (PNum b:[]) (PNum a) = PBool $ a > b

head' _ [] (PList (a:_)) = a
head' _ args val = argError "head" "List" args val

tail' _ [] (PList a) = PList $ tail a
tail' _ args value = argError "tail" "list, list" args value

prepend _ (value:[]) (PList a) = PList $ value : a

list _ args _ = PList args

quoteNames = map $ \(name, value) -> (PString name, value)

defaultExpressions =
    map (\(name, fn) -> (name, PRoutine $ PExpression fn)) $
    quoteNames [
        ("+", plus),
        ("*", multiply),
        ("<", lt),
        (">", gt),
        ("prepend", prepend),
        ("head", head'),
        ("tail", tail'),
        ("comment", identity),
        ("foldl", foldl'),
        (">>", \scope args value -> pipe scope ((PNum 1) : args) value),
        (">>>", \scope args value -> pipe scope ((PNum 2) : args) value),
        (">n", pipe),
        ("fn", fn),
        ("==", eq),
        ("!==", \scope args value -> not' scope [] (eq scope args value)),
        ("!", not'),
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
