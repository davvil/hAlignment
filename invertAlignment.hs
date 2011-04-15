module Main (main) where

import System.IO

import Alignment

invertAlignment :: Alignment -> Alignment
invertAlignment = map (\(x, y) -> (y, x))

main = do
    aFile <- hGetContents stdin
    let a = parseAlignmentFile aFile
    putStrLn $ unlines $ map printAlignment (map invertAlignment a)
