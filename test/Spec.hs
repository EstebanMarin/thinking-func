import Data.Char (toLower)
import Data.List (sort)
import Lib
import Test.QuickCheck

-- Assuming a sort function exists
-- sortFunction :: Ord a => [a] -> [a]

-- Property to test if the sortFunction sorts correctly
prop_sortsCorrectly :: [Int] -> Bool
prop_sortsCorrectly xs = sortFunction xs == sort xs

-- test lowecase
prop_lowercase :: Text -> Bool
prop_lowercase xs = lowercaseM xs == map toLower xs

-- prop_lowercase xs = lowercaseM xs == map toLower xs

main :: IO ()
main = do
  quickCheck prop_sortsCorrectly
  putStrLn "sortsCorrectly"
  quickCheck prop_lowercase
  putStrLn "lowercaseM"