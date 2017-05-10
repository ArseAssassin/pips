module REPL where

import System.Console.Haskeline
import Data.Char (isSpace)
import Control.Monad.IO.Class (liftIO)

import Utils (getDefaultScope)
import Lib (parsePIPs, runScript, PValue(PError, PAssignScope, PScope), defaultScope, mergeScopes)

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

main = do
    scope <- getDefaultScope
    runInputT defaultSettings $ loop scope
        where
           loop scope = do
                input <- getExpression

                case parsePIPs "REPL Input" (input ++ "\n") of
                    Left e -> do
                        outputStrLn "Error parsing input:"
                        outputStrLn (show e)
                        loop scope

                    Right it -> do
                        result <- liftIO (runScript it scope (PScope scope))
                        case result of
                            PError typeName it -> do
                                outputStrLn $ (show $ typeName) ++ ": \n" ++ it
                                outputStrLn ""
                                loop scope

                            PAssignScope newScope -> do
                                loop $ mergeScopes newScope scope

                            it -> do
                                outputStrLn $ "  " ++ (show it)
                                outputStrLn ""
                                loop scope
