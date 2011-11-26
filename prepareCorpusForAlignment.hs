{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import System.Console.CmdArgs
import System.IO

import Data.List
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as I

data SentencePair = SentencePair { 
    source::[T.Text]
    , target::[T.Text]
}
                    
data Options = Options {
    sourceFile::String
    , targetFile::String
    , sourceOutFile::String
    , targetOutFile::String
    , indexOutFile::String
    , maxLength::Int
    , ratio::Float
} deriving (Show, Data, Typeable)

optionsList = Options {
      sourceFile    = def &= help "source file" &= explicit &= name "s"
    , targetFile    = def &= help "target file" &= explicit &= name "t"
    , sourceOutFile = def &= help "source out file" &= explicit &= name "S"
    , targetOutFile = def &= help "target out file" &= explicit &= name "T"
    , indexOutFile  = def &= help "write indexes of suppressed lines to file"
    , maxLength     = 100 &= help "max lenght (default=100)" &= explicit &= name "l"
    , ratio         = 4   &= help "max ratio allowed between lengths"
}

writeCorpus :: Handle -> Handle -> Handle -> [([T.Text], [T.Text], Int)] -> IO ()
writeCorpus hs ht hi c = writeCorpus' hs ht hi c 0
    where 
        writeCorpus' _ _ _ [] _ = return ()
        writeCorpus' hs ht hi ((s,t,i):xs) lastLine = do
            I.hPutStrLn hs (T.unwords s)
            I.hPutStrLn ht (T.unwords t)
            writeMissingLines hi i lastLine
            --hPutStrLn hi (show i)
            writeCorpus' hs ht hi xs i

        writeMissingLines h currentLine lastLine = hPutStr h $ unlines $ map show [lastLine+1..currentLine-1]

filterCriterion::Int -> Float -> [T.Text] -> [T.Text] -> Bool
-- maxLength ratio source target
filterCriterion l r s t = let
        ls = length s
        lt = length t
        lsf = fromIntegral ls
        ltf = fromIntegral lt
    in
        (not $ null s) && (ls <= l) && (not $ null t) && (lt <= l) && (lsf / ltf < r) && (ltf / lsf < r)

main = do
    options <- cmdArgs optionsList
    source <- fmap (map T.words . T.lines) $ I.readFile (sourceFile options)
    target <- fmap (map T.words . T.lines) $ I.readFile (targetFile options)
    let corpus = zip3 source target [1..]
    let l = maxLength options
    let r = ratio options
    let filteredCorpus = filter (\(s,t,_) -> filterCriterion l r s t) corpus

    hs <- openFile (sourceOutFile options) WriteMode
    ht <- openFile (targetOutFile options) WriteMode
    hi <- openFile (indexOutFile options) WriteMode
    writeCorpus hs ht hi filteredCorpus
    hClose hs
    hClose ht
    hClose hi

    --I.writeFile (sourceOutFile options) (T.unlines $ map (T.unwords . fst) filteredCorpus)
    --I.writeFile (targetOutFile options) (T.unlines $ map (T.unwords . snd) filteredCorpus)
    
