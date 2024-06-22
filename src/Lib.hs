{-# LANGUAGE InstanceSigs #-}

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
    expBook,
    expSolution,
    date1,
    date2,
    addSum,
    palindrome,
  )
where

import Data.Char (isAlpha, toLower, toUpper)
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

-- here is a definition of the exponentiation function
expBook :: Integer -> Integer -> Integer
expBook x n
  | n == 0 = 1
  | n == 1 = x
  | otherwise = x * expBook x (n - 1)

expSolution :: Integer -> Integer -> Integer
expSolution x n
  | n == 0 = 1
  | n == 1 = x
  | even n = expSolution (x * x) m
  | odd n = x * expSolution x (m - 1)
  | otherwise = error "expSolution: unexpected input"
  where
    m = n `div` 2

-- exercise g: chapter 2

newtype DateBook = DateBook (Int, Int, Int)

instance Show DateBook where
  show :: DateBook -> String
  show (DateBook (d, m, y)) = dateSuffix d ++ " " ++ monthName m ++ ", " ++ show y

dateSuffix :: Int -> String
dateSuffix day
  | day `elem` [11, 12, 13] = show day ++ "th"
  | lastDigit == 1 = show day ++ "st"
  | lastDigit == 2 = show day ++ "nd"
  | lastDigit == 3 = show day ++ "rd"
  | otherwise = show day ++ "th"
  where
    lastDigit = day `mod` 10

monthName :: Int -> String
monthName month = case month of
  1 -> "January"
  2 -> "February"
  3 -> "March"
  4 -> "April"
  5 -> "May"
  6 -> "June"
  7 -> "July"
  8 -> "August"
  9 -> "September"
  10 -> "October"
  11 -> "November"
  12 -> "December"
  _ -> "Invalid month" -- Handles invalid month numbers

date1 :: DateBook
date1 = DateBook (1, 1, 2020)

date2 :: DateBook
date2 = DateBook (2, 2, 2020)

-- exercise h: chapter 2

type CIN = String

type CIN8 = String

addSum :: CIN8 -> CIN
addSum cin = cin ++ show (sumDigits `div` 10) ++ show (sumDigits `mod` 10)
  where
    sumDigits = sum $ map getDigit cin

getDigit :: Char -> Int
getDigit c = read [c]

valid :: CIN -> Bool
valid cin = sumDigits `mod` 10 == 0
  where
    sumDigits = sum $ map getDigit cin

-- exercise i: chapter 2
isPalindrome :: String -> Bool
isPalindrome s = ys == reverse ys
  where
    ys = map toLower $ filter isAlpha s

palindrome :: IO ()
palindrome = do
  putStrLn "Enter a string:"
  s <- getLine
  if isPalindrome s
    then putStrLn "Yes!"
    else putStrLn "No!"