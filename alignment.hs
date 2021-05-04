-- b) attachHeads attach the values h1 and h2 at the front of the two lists
-- respectively in each tuple in the tuple list. Example:
-- attachHeads 3 7 [([1,2,3], [4,5,6]), ([2,3,4],[5,6,7])]
--          == [([3,1,2,3],[7,4,5,6]), ([3,2,3,4],[7,5,6,7])]



scoreSpace = -1
scoreMismatch = -1
scoreMatch = 0


similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore xs [] = scoreSpace * length xs
similarityScore [] xs = scoreSpace * length xs
similarityScore (x:xs) (y:ys) = maximum' [(score x y)+(similarityScore xs ys), 
                                          (score x '-')+(similarityScore xs (y:ys)),
                                          (score '-' y)+(similarityScore (x:xs) ys)]

similarityScore :: String -> String -> (Int, AlignmentType)
similarityScore xs [] = (scoreSpace*length xs, (xs, replicate (length xs) '-'))
similarityScore [] xs = (scoreSpace*length xs, (replicate (length xs) '-'), xs)
similarityScore (x:xs) (y:ys) = (maximum' [fst allComb!!0, fst allComb!!1, fst allComb!!2], 
                                 \)--lägg till saker här
  where allComb = [similarityScore (x:xs) ys, similarityScore xs (y:ys), similarityScore xs ys]

maximum' :: (Ord a) => [a] -> a
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

score :: Char -> Char -> Int
score '-' _ = scoreSpace
score _ '-' = scoreSpace
score x y
  | x == y = scoreMatch
  | otherwise = scoreMismatch

-- b) 
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- c)
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
--maximaBy valueFnc xs = let value = (maximum . map valueFnc) xs
--                       in filter (\x -> valueFnc x == value) xs
maximaBy valueFnc xs = [x | x <- xs, valueFnc x == (maximum . map valueFnc) xs]

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments (x:xs) (y:ys) = 




