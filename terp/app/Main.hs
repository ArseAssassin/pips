module Main where

import System.Environment (getArgs)

import Text.ParserCombinators.Parsec
import Utils (getDefaultScope)
import Lib (parsePIPs, runScript, PValue(PError, PEffect, PScope, PString, PHashMap), defaultScope, unmeta, ASTNode, putInLib)

import Pipes

data Opts = ExecFile String | ProcessStdin String

exec (PEffect it) = do
    it >>= runEffect
exec (PString it) = putStr it
exec it = putStrLn (show it)

main :: IO ()
main = do
    args <- getArgs

    let command = if or [(elem "-s" args), (elem "--stdin" args)]
        then ProcessStdin (last args)
        else ExecFile (head args)

    execTerp command

execAst ast value = do
    case ast of
        Left e -> do putStrLn "Error parsing input:"
                     print e
        Right it -> do
            scope <- getDefaultScope
            result <- runScript it scope value

            exec result

execTerp :: Opts -> IO ()
execTerp (ExecFile filename) = do
    script <- readFile filename
    execAst (parsePIPs filename script) (PScope defaultScope)


execTerp (ProcessStdin script) = do
    input <- getContents
    let s = "'(= 'input _input), require 'lib/stdLib, import, to input, " ++ script ++ ", unmeta"
    execAst (parsePIPs "Command line arg" (s ++ "\n")) (PString input)
