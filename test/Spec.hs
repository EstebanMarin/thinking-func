import Data.List (sort)
import Lib (sortFunction)
import Test.QuickCheck

-- Assuming a sort function exists
-- sortFunction :: Ord a => [a] -> [a]

-- Property to test if the sortFunction sorts correctly
prop_sortsCorrectly :: [Int] -> Bool
prop_sortsCorrectly xs = sortFunction xs == sort xs

main :: IO ()
main = quickCheck prop_sortsCorrectly