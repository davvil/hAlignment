module Alignment(Alignment
                 , AlignedSentencePair
                 , source
                 , target
                 , alignment
                 , parseAlignmentFile
                 , parseFullCorpus
                 , alignmentRow
                 , alignmentColumn
                 , printAlignment
) where

type Alignment = [(Int, Int)]

data AlignedSentencePair = AlignedSentencePair {
    source :: [String]
    , target :: [String]
    , alignment :: Alignment
} deriving Show

parseAlignmentLine :: String -> Alignment
parseAlignmentLine = parseAlignmentLine' . words 
    where
        parseAlignmentLine' [] = []
        parseAlignmentLine' (x:xs) = (j, i) : parseAlignmentLine' xs
            where
                (jString, rest) = break (== '-') x
                j = read jString
                i = read $ tail rest

parseAlignmentFile :: String -> [Alignment]
parseAlignmentFile = map parseAlignmentLine . lines

parseFullCorpus :: String -> String -> String -> [AlignedSentencePair]
parseFullCorpus f e a = parseFullCorpus' (lines f) (lines e) (lines a)
    where
        parseFullCorpus' (f:fs) (e:es) (a:as) =
            AlignedSentencePair (words f) (words e) (parseAlignmentLine a) : parseFullCorpus' fs es as
        parseFullCorpus' _ _ _ = []

alignmentRow :: Alignment -> Int -> Alignment
alignmentRow a r = filter (\x -> snd x == r) a

alignmentColumn :: Alignment -> Int -> Alignment
alignmentColumn a c = filter (\x -> fst x == c) a

printAlignment :: Alignment -> String
printAlignment = foldr (\(x, y) s -> show x ++ "-" ++ show y ++ " " ++ s) ""
