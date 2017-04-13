module StdLib.Stream where

import Runtime
import StdLib.Helpers
import Conduit
import Data.ByteString.Char8 (unpack, pack)
import Data.Time.Clock.POSIX

pack' (PString it) =
    pack it

pack' value =
    pack $ show $ PError (PString "ValueError") $ "StdOut expects string as output, but received " ++ (show value)

readFile' _ [] (PString it) =
    PProducer $ sourceFileBS it .| mapC (PString . unpack)

mapStream scope [PFunction fn] (PProducer it) =
    PProducer $ it .| mapC (\it -> fn scope [] it)

zipStream _ [PProducer b] (PProducer a) =
    PProducer $ getZipSource $ (\a b -> PList [a, b]) <$> ZipSource a <*> ZipSource b

connect' _ [PConsumer b] (PProducer a) =
    PEffect $ a .| b

connect' _ args value =
    argError "sink" "Producer, Consumer" args value

take' _ [PNum i] (PProducer a) =
    PProducer $ a .| takeC i

drop' _ [PNum i] (PProducer a) =
    PProducer $ a .| dropC i

flatten' value = do
    case value of
        PProducer it -> toProducer it
        other -> yield $ PError (PString "ValueError") ("Expected flatMap to return (Producer), but returned " ++ (show other) ++ " instead")

flatMap scope [PFunction fn] (PProducer a) =
    PProducer $ a
        .| mapC (unmeta . (fn scope []))
        .| awaitForever flatten'

value _ [] value =
    PProducer $ yield value

slidingWindow' _ [PNum len] (PProducer it) =
    PProducer $ it .| slidingWindowC len .| mapC PList

skipBy scope [PFunction fn] (PProducer it) =
    PProducer $ it .| slidingWindowC 2 .| filterC (
        \[a, b] -> case fn scope [b] a of
                    PBool it -> not it
                    _ -> True
    ) .| mapC head

time' = liftIO (
        getPOSIXTime >>=
            (return . PNum . round . (* 1000)))
time =
    PProducer $ repeatMC time'

filterStream scope [PFunction fn] (PProducer it) =
    PProducer $ it .| filterC (
        \it -> case fn scope [] it of
                    PBool it -> it
                    _ -> True
    )

stdin' :: Runtime.Producer
stdin' = stdinC .| mapC (PString . unpack)

streamValues = quoteNames
    $ map (\(name, value) -> ("s." ++ name, value))
    [
        ("stdout", PConsumer $ mapC pack' .| stdoutC),
        ("stdin", PProducer $ stdinC .| mapC (PString . unpack)),
        ("random", PProducer $ sourceRandom .| mapC PNum),
        ("time", time),
        ("readFile", PFunction $ normalizeAll readFile'),
        ("map", PFunction $ normalizeAll mapStream),
        ("flatMap", PFunction $ normalizeAll flatMap),
        ("filter", PFunction $ normalizeAll filterStream),
        ("zip", PFunction $ normalizeAll zipStream),
        ("take", PFunction $ normalizeAll take'),
        ("drop", PFunction $ normalizeAll drop'),
        ("sink", PFunction $ normalizeAll connect'),
        ("slidingWindow", PFunction $ normalizeAll slidingWindow'),
        ("skipBy", PFunction $ normalizeAll skipBy),
        -- ("flatMap", PFunction $ normalizeAll flatMap),
        ("value", PFunction $ normalizeAll value)
    ]
