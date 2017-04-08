module Main where

import System.Environment (getArgs)

import Lib (parsePIPs, runScript, PValue(PError), defaultScope)

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
        Right it -> do
            result <- runScript it defaultScope
            putStrLn (show result)
