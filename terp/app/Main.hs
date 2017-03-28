module Main where

import System.Environment (getArgs)

import Lib (parsePIPs, runScript, PValue(PError))

main :: IO ()
main = do
    args <- getArgs
    let file = head args
    script <- readFile file
    let ast = parsePIPs file script
    -- putStrLn $ (show ast)
    case ast of
        Left e -> do putStrLn "Error parsing input:"
                     print e
        Right it -> putStrLn
            (case runScript it of
                PError typeName it -> (show typeName) ++ ": \n" ++ it
                it -> show it)
