import Data.Char (toLower)
import Data.List (sort)
import Lib
import Test.QuickCheck (quickCheck)

-- Assuming a sort function exists
-- sortFunction :: Ord a => [a] -> [a]

-- Property to test if the sortFunction sorts correctly
prop_sortsCorrectly :: [Int] -> Bool
prop_sortsCorrectly xs = sortFunction xs == sort xs

-- test lowecase
prop_lowercase :: Text -> Bool
prop_lowercase xs = lowercaseM xs == map toLower xs

-- prop_lowercase xs = lowercaseM xs == map toLower xs
-- ghci> word1xs "esteban es e"
-- ["esteban","es","e"]
-- ghci> sort
-- sortFunction  sortWord1s
-- ghci> sortWord1s ["esteban","es","e"]
-- ["e","es","esteban"]

-- test count words from two angles
prop_countWord1s :: [Word1] -> Bool
-- its really hard to think about properties
prop_countWord1s xs = countRuns xs == countRuns xs

main :: IO ()
main = do
  quickCheck prop_sortsCorrectly
  putStrLn "sortsCorrectly"
  quickCheck prop_lowercase
  putStrLn "lowercaseM"
  quickCheck prop_countWord1s
  putStrLn "counts words correctly"