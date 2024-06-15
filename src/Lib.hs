module Lib
  ( someFunc,
    sortFunction,
  )
where

import Data.List (sort)

sortFunction :: (Ord a) => [a] -> [a]
sortFunction = sort

someFunc :: IO ()
someFunc = putStrLn "someFunc"
