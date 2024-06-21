{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module Lib
  ( sortFunction,
    Word1,
    Text,
    commonWord1s,
    word1xs,
    lowercaseM,
    sortWord1s,
    someFunc,
    -- countWord1s,
    countRuns,
    showRun,
    sortRuns,
    modernise,
    lazySusan,
    firstT,
  )
where

import Data.Char (toLower, toUpper)
import Data.List (group, sort, sortBy)
import Data.Ord

sortFunction :: (Ord a) => [a] -> [a]
sortFunction = sort

type Word1 = String

type Text = String

-- model better the problem

-- devide the problem in smaller parts
-- lets have a function that takes a text and returns a list of word1s
word1xs :: Text -> [Word1]
-- we are assuming implementation from the standard library
word1xs = words

-- lowercase all the word1s
lowercaseM :: Text -> Text
lowercaseM = map toLower

-- sorting word1s
sortWord1s :: [Word1] -> [Word1]
sortWord1s = sort

-- count runs of the same word1s
-- countWord1s :: [Word1] -> [(Word1, Int)]
-- countWord1s [] = [] -- Add this line to handle the empty list case
-- countWord1s (x : xs) =
--   if x `elem` xs
--     then (x, length (filter (== x) xs) + 1) : countWord1s (filter (/= x) xs)
--     else (x, 1) : countWord1s (filter (/= x) xs)

-- faulty implementation

countRuns :: [Word1] -> [(Word1, Int)]
countRuns = map (\m -> (head m, length m)) . group

-- sort Runts
sortRuns :: [(Word1, Int)] -> [(Word1, Int)]
sortRuns = sortBy $ comparing $ Down . snd

-- show run
showRun :: (Word1, Int) -> String
showRun (w, n) = w ++ ": " ++ show n ++ "\n "

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- example common word1s
-- problem: find the most common word1 in a text
commonWord1s :: Int -> Text -> Word1
commonWord1s n =
  concatMap showRun . take n . sortRuns . countRuns . sortWord1s . word1xs . map toLower

-- exercise c: chapter 2

-- create a modernize function that takes a string and returns and
-- capitlizes the first letter of each word

modernise :: String -> String
modernise = unwords . map capitalize . words
  where
    capitalize (x : xs) = toUpper x : map toLower xs
    capitalize [] = []

-- exercise d: chapter 2
-- Eiger Beaver identity
-- instead of maping over a list and then taking the head
-- we can take the head of the list and then apply the function

eigerBeaver :: [b] -> (b -> a) -> a
eigerBeaver xs f = head $ map f xs
eigerBeaver xs f = f . head $ xs

-- say we want to add the filter function
filterM :: (a -> Bool) -> [a] -> [a]
filterM p = foldr (\x xs -> if p x then x : xs else xs) []

-- suzan with filter
lazySusan :: (Eq b) => b -> (b -> c) -> [b] -> c
lazySusan a f = f . head . filter (== a)

-- exercise e: chapter 2
firstT :: (a -> Bool) -> [a] -> Maybe a
firstT p xs = if null ys then Nothing else Just (head ys)
  where
    ys = filter p xs

-- exercise f: chapter 2
