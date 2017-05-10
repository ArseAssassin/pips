module Utils where

import System.Directory (getHomeDirectory, getCurrentDirectory)
import qualified Data.Map.Lazy as Map
import Lib (PValue(PString, PHashMap), defaultScope, putInLib)

getDefaultScope = do
    homeDir <- getHomeDirectory
    cwd <- getCurrentDirectory
    return $ putInLib
                (PString "libraryPath")
                 (PHashMap $ Map.fromList [(PString "lib", PString $ homeDir ++ "/.pips/0.0.1/lib"),
                                           (PString ".", PString $ cwd),
                                           (PString "..", PString $ cwd ++ "/.." )])
                 defaultScope
