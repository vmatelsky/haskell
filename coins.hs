module Coins
where

data Coin = Good | Fake Bool deriving (Show)

generateSequence :: Int -> Int -> [Coin]
generateSequence coinsNumber fakePosition 
 | coinsNumber <= 0 = []
 | coinsNumber == fakePosition = nextIteration ++ [Fake True]
 | otherwise = nextIteration ++ [Good]
 where nextIteration = generateSequence (coinsNumber - 1) fakePosition

generateThirteen :: Int -> [Coin]
generateThirteen fakePosition = generateSequence 13 fakePosition

-- generate all possible positions of fake coin
allThirteenSequences :: [[Coin]]
allThirteenSequences = map generateThirteen [1..13]

foldCoinsSet :: [Coin] -> Coin
foldCoinsSet [] = Good
foldCoinsSet xs = foldr (\x acc -> if x == Fake then x else acc) Good xs

-- isFirstSetLess :: [Coin] -> [Coin] -> Bool
-- isFirstSetLess [] [] = false
-- isFirstSetLess [] _ = true
-- isFirstSetLess _ [] = false
-- isFirstSetLess first second
--  | length first != length secont = error "Coins sets should be the -- same size"

-- step one
-- compare first five with second five
