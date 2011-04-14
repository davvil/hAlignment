module Main (main) where

import System.Environment
import System.IO

import Alignment
import AsciiAlignment

mainLoop :: [AlignedSentencePair] -> Int -> IO()
mainLoop corpus currentSentence = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case input of
        "" -> (putStrLn $ drawAlignment (corpus!!currentSentence)) >> mainLoop corpus (currentSentence+1)
        "q" -> putStrLn "Quit"
        otherwise -> do
            let newSentence = (read input::Int) - 1
            putStrLn $ drawAlignment (corpus!!newSentence)
            mainLoop corpus (newSentence+1)

main = do
    args <- getArgs
    f <- readFile (args!!0)
    e <- readFile (args!!1)
    a <- readFile (args!!2)

    let corpus = parseFullCorpus f e a
    --let pic = drawAlignment (corpus!!n)
    --putStrLn pic
    mainLoop corpus 0
