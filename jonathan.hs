-- a
similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore _ [] = scoreSpace
similarityScore [] _ = scoreSpace
similarityScore (x:xs) (y:ys) = max' [scoreChar x y + similarityScore xs ys,
                                      scoreChar '-' y + similarityScore (x:xs) ys,
                                      scoreChar x '-' + similarityScore xs (y:ys)]

scoreChar :: Char -> Char -> Int
scoreChar x y
 | x == y    = scoreMatch
 | x == '-'  = scoreSpace
 | y == '-'  = scoreSpace
 | otherwise = scoreMismatch

max' :: Ord a => [a] -> a
max' (x:xs) 
 | xs /= []  = max x $ max' xs
 | otherwise = x

-- b
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- c 
maximaBy :: Ord b => (a->b) -> [a] -> [a]
maximaBy f l = [ x | x <- l, f x == max' (map f l)]  

--d
type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2 =
    maximaBy score $ generate string1 string2
    --generate string1 string2
    where generate [] [] = [([], [])]  -- this generates all possible alignments for string1 and string2 (within not so much reason)
          generate [] (y:ys) = attachHeads '-' y $ generate [] ys 
          generate (x:xs) [] = attachHeads x '-' $ generate xs [] 
          generate (x:xs) (y:ys) = (attachHeads x y $ generate xs ys) ++ (attachHeads '-' y $ generate (x:xs) ys) ++ (attachHeads x '-' $ generate xs (y:ys))


scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1 

score :: AlignmentType -> Int
score ([],[]) = 0
score ((x:xs),(y:ys)) = score (xs,ys) + scoreChar x y
