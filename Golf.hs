module Golf where
import Data.List
{-
Create a historigram from a list.
Count the number of instance of each number between 0 and 9.
  *         * 
  * *       *
===================
0 1 2 3 4 5 6 7 8 9
-}

count :: [Int] -> Int -> Int
count [] _   = 0
count (x:xs) n
  |n == x    = 1 + count xs n
  |otherwise = count xs n

numberInstance :: [Int] -> [Int]
numberInstance l = map (count l) [0..9]

stars :: [Int] -> [String]
stars l = map (\x -> replicate (maximum l - x) ' ' ++ replicate x '*') l ++ [replicate (maximum l) '\n']

histograme :: [Int] -> String
histograme l = concat (transpose (stars (numberInstance l))) ++ "==========\n" ++ "0123456789\n"
