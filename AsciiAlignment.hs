module AsciiAlignment where

import Alignment

drawAlignment :: AlignedSentencePair -> String
drawAlignment s = drawAlignmentBody s maxTargetWidth ++
                  drawSeparatorLine maxTargetWidth sourceLength ++
                  drawSource s maxTargetWidth

    where
        maxTargetWidth = maximum $ map length $ target s
        sourceLength = length $ source s

drawAlignmentBody :: AlignedSentencePair -> Int -> String
drawAlignmentBody s w = drawAlignmentBody' ((length $ target s) - 1) (reverse $ target s) (alignment s) w
    where
        drawAlignmentBody' _ [] _ _ = ""
        drawAlignmentBody' pos (e:es) a w = let 
                interestingPoints = alignmentRow a pos
                sourceLength = length $ source s
            in
                drawAlignmentBodyLine e interestingPoints sourceLength w ++ drawAlignmentBody' (pos-1) es a w

drawAlignmentBodyLine :: String -> Alignment -> Int -> Int -> String
drawAlignmentBodyLine e a sourceWidth maxTargetWidth = (rightPadded maxTargetWidth e) ++ " |" ++ (drawAlignmentPoints a sourceWidth) ++ "\n"

drawAlignmentPoints :: Alignment -> Int -> String
drawAlignmentPoints a w = drawAlignmentPoints' a 0 w
    where
        drawAlignmentPoints' a p w = if p == w then
                     ""
                else
                     " " ++ thisPoint ++ drawAlignmentPoints' a (p+1) w
            where
                thisPoint = if null $ alignmentColumn a p then "." else "#"

rightPadded :: Int -> String -> String
rightPadded w x = padding ' ' (w - (length x)) ++ x

leftPadded :: Int -> String -> String
leftPadded w x = x ++ padding ' ' (w - (length x))

padding c 0 = ""
padding c x = c : padding c (x-1)

drawSeparatorLine :: Int -> Int -> String
drawSeparatorLine maxTargetLength sourceLength = padding ' ' (maxTargetLength+1) ++ "|" ++ padding '-' (2*sourceLength+1) ++ "\n"

drawSource :: AlignedSentencePair -> Int -> String
drawSource a w = unlines $ map ((padding ' ' (w+2)) ++) $ transpose $ map (leftPadded maxSourceWidth) fWithSpaces
    where
        f = source a
        maxSourceWidth = maximum $ map length f
        fWithSpaces = foldr ((++).("":).(:[])) [] f

--transpose :: [[a]] -> [[a]]
transpose [] = [] 
transpose ([]:xs) = []
transpose l = firstLine : transpose rest
    where
        (firstLine, rest) = transposeColumn l

--transposeColumn :: [[a]] -> ([a], [[a]])
transposeColumn [] = ([], [])
transposeColumn ((x:xs):l) = ((x:line), (xs:rest))
    where
        (line, rest) = transposeColumn l
