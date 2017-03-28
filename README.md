# PIPs

PIPs is an experimental high-level programming language written in Haskell based around the idea of a language where every function is called using infix notation.

`>> to 0 range 10 map '(* 2)`

## Examples

```
comment Hello world!,
to "Hello world!"
```

```
comment FizzBuzz,
= 'fizzBuzz '(
    >> to (mod 3, == 0) and (mod 5, == 0) and 'FizzBuzz
       or (mod 3, == 0) and 'Fizz
       or (mod 5, == 0) and 'Buzz
       or (str)
),
>> to 1 range 50 map fizzBuzz join " "
```

## Try it out

* Get [Stack](https://docs.haskellstack.org/en/stable/README/)
* `git clone git@github.com:ArseAssassin/pips.git`
* `cd pips/terp`
* `stack build`
* `stack exec pips-repl`

## Syntax

```
% comment function discards everything until it is terminated
PScope

% comment multiple terms are separated using a comma, comment like this
PScope

% comment 'to discards the left side of the function call and returns its argument, to "This is returned"
"This is returned"

% comment 2 and -4 are examples of numbers, to -10
-10

% comment use "double quotes" for strings, to "this is a string"
"this is a string"

% comment or a single quote for a word, to 'word
"word"

% comment use the 'list function to define a list, list 1 2 3
(list 1 2 3)

% comment quote before parenthesis creates a new function, list 1 2 3, map '(* 2)
(list 2 4 6)

% comment '= is a function that takes two arguments; name for the assignment and the value, = 'foo 2, to 1, + foo
3

% = 'inc '(+ 1), to 0, range 10, map inc
(list 1 2 3 4 5 6 7 8 9 10 11)

% comment '= can also take multiple assignments, = 'foo 1 'bar '(+ foo), to 1, bar
2

% comment '>> is a function that takes pairs of functions and arguments and applies them from left to right
PScope

% >> to 0 range 10 filter '(> 4)
(list 5 6 7 8 9 10)

% comment metaprogramming is done without macros, = 'setFoo '(= 'foo args), setFoo 2, to foo
(list 2)

% comment metadata can be attached to all values, = 'foo (to 'value, meta 'metaDataField 'metaDataValue)
% to foo, meta 'metaDataField
"metaDataValue"

% comment for example names are assigned to all assigned names, to foo, meta 'name
"foo"
```
