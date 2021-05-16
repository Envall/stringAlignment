import Data.List (intersperse)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1 

-- a
similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore xs [] = scoreSpace * length xs
similarityScore [] ys = scoreSpace * length ys
similarityScore (x:xs) (y:ys) = max' [charToScore x y + similarityScore xs ys,
                                      charToScore '-' y + similarityScore (x:xs) ys,
                                      charToScore x '-' + similarityScore xs (y:ys)]

charToScore :: Char -> Char -> Int
charToScore x y
 | x == y    = scoreMatch
 | x == '-'  = scoreSpace
 | y == '-'  = scoreSpace
 | otherwise = scoreMismatch

max' :: Ord a => [a] -> a
max' (x:xs) 
 | xs == []  = x
 | otherwise = max x $ max' xs

-- b
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- c 
maximaBy :: Ord b => (a->b) -> [a] -> [a]
maximaBy f l = [ x | x <- l, f x == maximum (map f l)]  

--d
type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments [] (y:ys) = attachHeads '-' y $ optAlignments [] ys
optAlignments (x:xs) [] = attachHeads x '-' $ optAlignments xs []
optAlignments (x:xs) (y:ys) = maximaBy score  (attachHeads x y (optAlignments xs ys) ++
                                               attachHeads x '-' (optAlignments xs (y:ys)) ++
					       attachHeads '-' y (optAlignments (x:xs) ys))


score :: AlignmentType -> Int
score ([],[]) = 0
score ((x:xs),(y:ys)) = score (xs,ys) + charToScore x y

-- e
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
    let res = optAlignments string1 string2
    putStrLn $ "There are " ++ (show . length) res ++ " optimal alignments:"
    putStrLn ""
    mapM printAlignments res
    putStrLn $ "There were " ++ (show . length) res ++ " optimal alignments!"

printAlignments :: AlignmentType -> IO ()
printAlignments x = do
    putStrLn $ (intersperse ' ' . fst) x
    putStrLn $ (intersperse ' ' . snd) x
    putStrLn ""


-- 3. Optimization
attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs++[h1], ys++[h2]) | (xs,ys) <- aList]

mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> Int
    mcsEntry _ 0 = 0
    mcsEntry 0 _ = 0
    mcsEntry i j
      | x == y    = 1 + mcsLen (i-1) (j-1)
      | otherwise = max (mcsLen i (j-1)) 
                        (mcsLen (i-1) j)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)


similarityScore' :: String -> String -> Int
similarityScore' xs ys = tableScore (length xs) (length ys)
  where
    tableScore i j = table!!i!!j
    table = [[ tableEntry i j | j<-[0..]] | i<-[0..] ]
       
    tableEntry :: Int -> Int -> Int
    tableEntry 0 0 = 0
    tableEntry i 0 = scoreSpace + tableScore (i-1) 0 
    tableEntry 0 j = scoreSpace + tableScore 0 (j-1)
    tableEntry i j
      | x == y    = scoreMatch + tableScore (i-1) (j-1)
      | otherwise = maximum [scoreSpace + tableScore i (j-1),
                             scoreSpace + tableScore (i-1) j,
			     scoreMismatch + tableScore (i-1) (j-1)]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

optAlignments' :: String -> String -> [AlignmentType]
optAlignments' xs ys = snd $ tableScore (length xs) (length ys)
  where
    tableScore i j = table!!i!!j
    table = [[ tableEntry i j | j<-[0..]] | i<-[0..]]

    tableEntry :: Int -> Int -> (Int, [AlignmentType])
    tableEntry 0 0 = (0, [([],[])])
    tableEntry i j
     | i == 0    = (scoreSpace + fst nxtUp,   attachTails '-' y (snd nxtUp)) 
     | j == 0    = (scoreSpace + fst nxtLeft, attachTails x '-' (snd nxtLeft))
     | x == y    = (scoreMatch + fst nxtDiag, attachTails x y (snd nxtDiag))
     -- Collect all tuples with prefixes that give the max score and concatenate them to a single tuple
     | otherwise = concatTuple $ maximaBy (\t->fst t) [(scoreSpace + fst nxtUp, attachTails '-' y (snd nxtUp)),
                                                       (scoreSpace + fst nxtLeft, attachTails x '-' (snd nxtLeft)),
				                       (scoreMismatch + fst nxtDiag, attachTails x y (snd nxtDiag))]
      where 
        x = xs!!(i-1)
    	y = ys!!(j-1)
        nxtUp   = tableScore i (j-1)
	nxtLeft = tableScore (i-1) j
	nxtDiag = tableScore (i-1) (j-1)

concatTuple :: [(Int,[AlignmentType])] -> (Int,[AlignmentType])
concatTuple (x:[])   = (fst x, snd x) 
concatTuple (x:xs) = (fst x, snd x ++ (snd . concatTuple) xs) 

outputOptAlignments' :: String -> String -> IO ()
outputOptAlignments' string1 string2 = do
    let res = optAlignments' string1 string2
    putStrLn $ "There are " ++ (show . length) res ++ " optimal alignments:"
    putStrLn ""
    mapM printAlignments res
    putStrLn $ "There were " ++ (show . length) res ++ " optimal alignments!"


