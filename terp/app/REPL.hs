module REPL where

import System.Console.Haskeline
import Data.Char (isSpace)
import Control.Monad.IO.Class (liftIO)

import Lib (parsePIPs, runScript, PValue(PError, PAssignScope), defaultScope)

trim = f . f
    where f = reverse . dropWhile isSpace

getExpression = do
    minput <- getInputLine "% "
    case minput of
        Nothing -> return ""
        Just it ->
            if last (trim it) == ','
                then do
                    next <- getExpression
                    return $ it ++ next
                else return it

main = runInputT defaultSettings $ loop defaultScope
   where
       loop scope = do
            input <- getExpression

            case parsePIPs "REPL Input" (input ++ "\n") of
                Left e -> do
                    outputStrLn "Error parsing input:"
                    outputStrLn (show e)
                    loop scope

                Right it -> do
                    result <- liftIO (runScript it scope)
                    case result of
                        PError typeName it -> do
                            outputStrLn $ (show $ typeName) ++ ": \n" ++ it
                            outputStrLn ""
                            loop scope

                        PAssignScope newScope -> do
                            loop $ newScope ++ scope

                        it -> do
                            outputStrLn $ show $ it
                            outputStrLn ""
                            loop scope
