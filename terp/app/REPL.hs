module REPL where

import System.Console.Haskeline

import Lib (parsePIPs, runScript, PValue(PError, PAssignScope), defaultScope, unmeta)

main = runInputT defaultSettings $ loop defaultScope
   where
       loop scope = do
           minput <- getInputLine "% "

           case minput of
                Nothing -> return ()
                Just input ->
                    case parsePIPs "REPL Input" (input ++ "\n") of
                        Left e -> do
                            outputStrLn "Error parsing input:"
                            outputStrLn (show e)
                            loop scope

                        Right it ->
                            case runScript it scope of
                                PError typeName it -> do
                                    outputStrLn $ (show $ typeName) ++ ": \n" ++ it
                                    loop scope

                                PAssignScope scope -> do
                                    loop scope

                                it -> do
                                    outputStrLn $ show $ unmeta it
                                    loop scope
