module Main where

import Lib (parsePIPs, runScript)

main :: IO ()
main = do
    script <- readFile "../examples/test.pip"
    let ast = parsePIPs "../examples/test.pip" script
    -- putStrLn $ (show ast)
    case ast of
        Left e -> do putStrLn "Error parsing input:"
                     print e
        Right it -> putStrLn $ show $ runScript it
