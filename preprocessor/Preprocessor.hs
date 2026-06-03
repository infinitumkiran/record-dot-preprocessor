
module Preprocessor(main) where

import Edit
import System.IO.Extra
import System.Environment


-- GHC calls me with: original input output <any extra arguments>
-- Test calls me with: --test directory
-- Users call me with: input
--
-- The @--no-hasfield@ flag disables generation of HasField instances (keeping
-- the dot-syntax rewrites). Pass it via cabal as @-optF--no-hasfield@. It may
-- appear anywhere in the argument list, so we strip it before positional parsing.
main :: IO ()
main = do
    rawArgs <- getArgs
    let noHasField = "--no-hasfield" `elem` rawArgs
        args = filter (/= "--no-hasfield") rawArgs
    case args of
        original:input:output:_ -> runConvert noHasField original input output
        input:output:_ -> runConvert noHasField input input output
        input:_ -> runConvert noHasField input input "-"
        [] -> putStrLn "record-dot-preprocess [--no-hasfield] [FILE-TO-CONVERT]"


runConvert :: Bool -> FilePath -> FilePath -> FilePath -> IO ()
runConvert noHasField original input output = do
    res <- recordDotPreprocessor noHasField original <$> readFileUTF8' input
    if output == "-" then putStrLn res else writeFileUTF8 output res
