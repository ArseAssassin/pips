module StdLib.Stream where

import Runtime
import StdLib.Helpers
import Data.ByteString.Char8 (unpack, pack)
import Data.Time.Clock.POSIX
import Pipes
import Pipes.Concurrent
import System.IO (isEOF)
import System.Random
import Control.Monad (unless)

-- pack' (PString it) =
--     pack it

-- pack' value =
--     pack $ show $ PError (PString "ValueError") $ "StdOut expects string as output, but received " ++ (show value)

-- readFile' _ [] (PString it) =
--     PProducer $ sourceFileBS it .| mapC (PString . unpack)

-- mapStream scope [PFunction fn] (PProducer it) =
--     PProducer $ it .| mapC (\it -> fn scope [] it)

-- zipStream _ [PProducer b] (PProducer a) =
--     PProducer $ getZipSource $ (\a b -> PList [a, b]) <$> ZipSource a <*> ZipSource b

-- connect' _ [PConsumer b] (PProducer a) =
--     PEffect $ a .| b

-- connect' _ args value =
--     argError "sink" "Producer, Consumer" args value

-- take' _ [PNum i] (PProducer a) =
--     PProducer $ a .| takeC i

-- drop' _ [PNum i] (PProducer a) =
--     PProducer $ a .| dropC i

-- flatten' value = do
--     case value of
--         PProducer it -> toProducer it
--         other -> yield $ PError (PString "ValueError") ("Expected flatMap to return (Producer), but returned " ++ (show other) ++ " instead")

-- flatMap scope [PFunction fn] (PProducer a) =
--     PProducer $ a
--         .| mapC (unmeta . (fn scope []))
--         .| awaitForever flatten'

-- value _ [] value =
--     PProducer $ yield value

-- slidingWindow' _ [PNum len] (PProducer it) =
--     PProducer $ it .| slidingWindowC len .| mapC PList

-- skipBy scope [PFunction fn] (PProducer it) =
--     PProducer $ it .| slidingWindowC 2 .| filterC (
--         \[a, b] -> case fn scope [b] a of
--                     PBool it -> not it
--                     _ -> True
--     ) .| mapC head

-- time' = liftIO (
--         getPOSIXTime >>=
--             (return . PNum . round . (* 1000)))
-- time =
--     PProducer $ repeatMC time'

-- filterStream scope [PFunction fn] (PProducer it) =
--     PProducer $ it .| filterC (
--         \it -> case fn scope [] it of
--                     PBool it -> it
--                     _ -> True
--     )

stdout :: PConsumer
stdout = do
    value <- await
    case value of
        PString it -> do
            lift $ putStr it
            stdout
        other -> do
            lift $ putStr $ show $ PThrown (PError (PString "ValueError") $ "stdout expects string as output, received instead " ++ (show other))
            stdout

-- print' :: PConsumer
-- print' = do
--     value <- await
--     lift $ putStrLn $ show value
--     print'

-- stdin :: PProducer
-- stdin = do
--     eof <- lift isEOF
--     unless eof $ do
--         str <- lift getContents
--         yield $ PString str
--         stdin

-- readFile' _ [] (PString name) =
--     PProducer prod
--     where prod = do
--             str <- lift $ readFile name
--             yield $ PString str


sink :: Scope -> [PValue] -> PValue -> PValue
sink _ [PConsumer b] (PProducer a) = PEffect $ do
    prod <- a
    return $ prod >-> b

-- sink _ [PConsumer b] (PMultiplex a) = PMultiplex $ do
--     sn <- a
--     case sn of
--         PProducer it -> return $ PEffect $ it >-> b

value' _ [] = PProducer . return . yield

-- mapStream scope [PFunction fn] (PProducer it) = PProducer $
--     it >-> iter
--     where
--         iter = do
--             value <- await
--             yield $ fn scope [] value
--             iter

-- filterStream scope [PFunction fn] (PProducer it) = PProducer $
--     it >-> iter
--     where
--         iter = do
--             value <- await
--             case fn scope [] value of
--                 PBool True -> yield value
--                 PBool False -> return ()
--                 other -> yield $ PThrown $ PError (PString "ValueError") $ "filter expects fn to return boolean on invocation, returned instead: " ++ (show other)

--             iter

-- values _ [] (PList it) = PProducer $ do
--     mapM yield it
--     return ()

-- value' _ [] it = PProducer $ yield it

-- random' = do
--     it <- lift randomIO
--     yield $ PNum it
--     random'

-- time' = do
--     it <- lift getPOSIXTime
--     yield $ PNum $ round (it * 1000)
--     time'

merge' _ [PProducer b] (PProducer a) = PProducer $ do
    (output, input) <- spawn Single
    aa <- a
    bb <- b
    let a' = aa >-> toOutput output
        b' = bb >-> toOutput output
        in return $ fromInput input

streamValues = quoteNames
    $ map (\(name, value) -> ("s." ++ name, value))
    [
        ("stdout", PConsumer stdout),
        -- ("print", PConsumer print'),
        -- ("stdin", PProducer stdin),
        ("sink", PFunction $ normalizeAll sink),
    --     ("values", PFunction $ normalizeAll values),
        ("value", PFunction $ normalizeAll value'),
        ("merge", PFunction $ normalizeAll merge')
    --     ("random", PProducer random'),
    --     ("time", PProducer time'),
    --     ("readFile", PFunction $ normalizeAll readFile'),
    --     ("map", PFunction $ normalizeAll mapStream),
    --     ("filter", PFunction $ normalizeAll filterStream),
    --     -- ("flatMap", PFunction $ normalizeAll flatMap),
    --     -- ("filter", PFunction $ normalizeAll filterStream),
    --     -- ("zip", PFunction $ normalizeAll zipStream),
    --     -- ("take", PFunction $ normalizeAll take'),
    --     -- ("drop", PFunction $ normalizeAll drop'),
    --     -- ("sink", PFunction $ normalizeAll connect'),
    --     -- ("slidingWindow", PFunction $ normalizeAll slidingWindow'),
    --     -- ("skipBy", PFunction $ normalizeAll skipBy),
    --     -- -- ("flatMap", PFunction $ normalizeAll flatMap),
    --     ("value", PFunction $ normalizeAll value)
    ]
