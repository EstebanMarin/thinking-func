module Lib
  ( sortFunction,
    Word1,
    Text,
    commonWord1s,
    word1xs,
    lowercaseM,
    sortWord1s,
    someFunc,
  )
where

import Data.Char (toLower)
import Data.List (sort)

sortFunction :: (Ord a) => [a] -> [a]
sortFunction = sort

type Word1 = [Char]

type Text = [Char]

-- example common word1s
-- problem: find the most common word1 in a text
commonWord1s :: Int -> Text -> Word1
commonWord1s n = undefined

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
