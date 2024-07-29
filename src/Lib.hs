{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lib where

import Control.Applicative (optional)
import qualified Control.Monad
import Data.List (group, sort, sortBy)
import Data.List.NonEmpty (nonEmpty)
import Data.Ord
import GHC.Unicode
import Text.ParserCombinators.ReadPrec (reset)

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

-- eigerBeaver xs f = f . head $ xs

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

-- chapter 4 Notes
testUntil :: Integer
testUntil = naiveUntil (> 100) (* 7) 1

-- ghci> testUntil2 1
-- 343

naiveUntil :: (a -> Bool) -> (a -> a) -> a -> a
naiveUntil p f x = if p x then x else naiveUntil p f (f x)

-- naive floor it takes n steps to reach the floor of a float
-- very inneficient

naiveFloor :: Float -> Integer
naiveFloor x =
  if x < 0
    then
      -- until (\n -> fromInteger n < x) (subtractM 1) (-1)
      until ((<= x) . fromInteger) (subtractM 1) (-1)
    else until ((> x) . fromInteger) (+ 1) 1 - 1
  where
    subtractM a b = a - b

binaryFloor :: Float -> Integer
binaryFloor x = fst $ until unit (shrink x) (bound x)
  where
    unit (a, b) = a + 1 == b
    shrink y (a, b) = if y < fromInteger m then (a, m) else (m, b)
      where
        m = (a + b) `div` 2
    bound y = (lower y, upper y)
      where
        lower a = until ((<= a) . fromInteger) (* 2) (-1)
        upper a = until ((> a) . fromInteger) (* 2) 1

-- natural numbers

data Nat = Zero | Succ Nat

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  Zero == Zero = True
  Succ n == Succ m = n == m
  _ == _ = False

instance Show Nat where
  show :: Nat -> String
  show Zero = "Zero"
  show (Succ Zero) = "Succ Zero"
  show (Succ (Succ n)) = "Succ (Succ " ++ show n ++ ")"

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  Zero + n = n
  Succ m + n = Succ (m + n)
  (*) :: Nat -> Nat -> Nat
  Zero * _ = Zero
  Succ m * n = n + m * n
  abs :: Nat -> Nat
  abs = id
  signum :: Nat -> Nat
  signum Zero = Zero
  signum _ = Succ Zero
  fromInteger :: Integer -> Nat
  fromInteger n
    | n < 0 = error "fromInteger: negative"
    | n == 0 = Zero
    | otherwise = Succ (fromInteger (n - 1))

  negate :: Nat -> Nat
  negate = error "negate is not defined for Nat"

-- exercise 3 F

sqrtBookNewton :: Float -> Float
sqrtBookNewton x = until goodEnough improve x
  where
    goodEnough y = abs (y * y - x) < eps * x
    improve y = (y + x / y) / 2
    eps = 0.0001

-- ghci> sqrtBookNewton 2
-- 1.4142157

-- exercise 3 G

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  Zero <= Zero = False
  Zero <= Succ _ = True
  Succ n <= Succ m = n <= m
  _ <= Zero = False

-- notes on chapter 4 list
-- workhorse of functional programmingo

list1 :: [Integer]
list1 = [x * x | x <- [1 .. 10]]

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\d -> n `mod` d /= 0) [2 .. n - 1]

list2 :: [Integer]
list2 = [x * x | x <- [1 .. 5], isPrime x]

list3 :: [(Integer, Integer)]
list3 = [(i, j) | i <- [1 .. 5], even i, j <- [i .. 5]]

-- common functions

mapMine :: (t -> a) -> [t] -> [a]
mapMine f xs = [f x | x <- xs]

filterMine :: (a -> Bool) -> [a] -> [a]
filterMine p xs = [x | x <- xs, p x]

concatMine :: [[a]] -> [a]
concatMine xss = [x | xs <- xss, x <- xs]

headMine :: [a] -> a
headMine [] = error "head of empty list"
headMine (x : _) = x

tailMine :: [a] -> [a]
tailMine [] = error "tail of empty list"
tailMine (_ : xs) = xs

lastMine :: [a] -> a
lastMine [] = error "last of empty list"
lastMine [x] = x
lastMine (_ : xs) = lastMine xs

-- homegrow implementation of the concatetation simbol

(++!) :: [a] -> [a] -> [a]
[] ++! ys = ys
(x : xs) ++! ys = x : (xs ++! ys)

-- noticed that
-- ghci> undefined ++! [1,2]

-- *** Exception: Prelude.undefined

-- CallStack (from HasCallStack):
--   undefined, called at <interactive>:6:1 in interactive:Ghci2

lengthMine :: [a] -> Integer
lengthMine [] = 0
lengthMine (_ : xs) = 1 + lengthMine xs

concatMine2 :: [[a]] -> [a]
concatMine2 = foldr (++!) []

mapMine2 :: (a -> b) -> [a] -> [b]
mapMine2 = map

-- ghci> concatMine2 [[1,2],[3,4]]
-- [1,2,3,4]

-- exercise 4 B
allPairsPermutatedUnique :: [(Integer, Integer)]
allPairsPermutatedUnique = [(x, y) | x <- [1 ..], y <- [1 ..], x < y]

allPairsPermutatedUnique2 :: [(Integer, Integer)]
allPairsPermutatedUnique2 = [(0, y) | y <- [1 ..]]

-- exercise 4 C

disjointImple :: (Ord a) => [a] -> [a] -> Bool
disjointImple xs ys = null [x | x <- xs, y <- ys, x == y]

disjointImpleBetter :: (Ord a) => [a] -> [a] -> Bool
disjointImpleBetter _ [] = True
disjointImpleBetter [] _ = True
disjointImpleBetter xs'@(x : xs) ys'@(y : ys)
  | x < y = disjointImpleBetter xs ys'
  | x > y = disjointImpleBetter xs' ys
  | otherwise = False

-- exercise 4 D
-- under what conditions funtions 1 and 2 produce
-- the same result

-- funtion 1
-- function1 = [e | x <- xs, p x, y <- ys]
-- say y <- ys is an infinite list to laziness we need to be aware about
-- the cost of folding such data structures

-- exercise 4 E
-- ramanujan numbers
-- a number that can be expressed as the sum of two cubes in two different ways
-- a^3 + b^3 = c^3 + d^3
-- ramanujanNumers n :: (Num a, Eq a, Enum a) => [(a, a, a, a)]
ramanujanNumers :: Integer -> [(Integer, Integer, Integer, Integer)]
ramanujanNumers n =
  [ (a, b, c, d)
    | a <- [1 .. n],
      b <- [a .. n],
      c <- [a + 1 .. n],
      d <- [c .. n],
      (a :: Integer) ^ (3 :: Integer) + (b :: Integer) ^ (3 :: Integer)
        == (c :: Integer) ^ (3 :: Integer) + (d :: Integer) ^ (3 :: Integer)
  ]

--   ghci> ramanujanNumers 12
-- [(1,12,9,10)]

-- exercise 4 G
-- recursive length definition it takes n space
-- for each element in the list
lengthRec :: [a] -> Integer
lengthRec [] = 0
lengthRec (_ : xs) = 1 + lengthRec xs

-- tail recursive length definition
-- it takes constant space for each element in the list
lengthTailRec :: [a] -> Integer
lengthTailRec xs = lengthTailRec' xs 0
  where
    lengthTailRec' [] n = n
    lengthTailRec' (_ : xss) n = lengthTailRec' xss (n + 1)

-- exercise 4 H
-- recursive definition of take and drop
takeRec :: Integer -> [a] -> [a]
takeRec 0 _ = []
takeRec _ [] = []
takeRec n (x : xs) = x : takeRec (n - 1) xs

-- tail recursive definition of take
takeTailRec :: Integer -> [a] -> [a]
takeTailRec n xs = takeTailRec' n xs []
  where
    takeTailRec' 0 _ acc = reverse acc
    takeTailRec' _ [] acc = reverse acc
    takeTailRec' n' (x : xss) acc = takeTailRec' (n' - 1) xss (x : acc)

-- recursive definition of drop
dropRec :: Integer -> [a] -> [a]
dropRec 0 xs = xs
dropRec _ [] = []
dropRec n (_ : xs) = dropRec (n - 1) xs

-- tail recursive definition of drop
dropTailRec :: Integer -> [a] -> [a]
dropTailRec n xs = dropTailRec' n xs []
  where
    dropTailRec' 0 xs' _ = xs'
    dropTailRec' _ [] acc = reverse acc
    dropTailRec' n' (x : xss) acc = dropTailRec' (n' - 1) xss (x : acc)

-- exercise 4 I
-- map (f . g) xs = map f (map g xs)

forkEx :: (a -> b, a -> c) -> a -> (b, c)
forkEx (f, g) x = (f x, g x)

-- chapter 5 notes sudoku solver

-- where are missing preciseness in the type signature
-- the matrix is a list of m rows where each row has the same length n
-- haskell cannot express this constraint in the type system
-- there are dependently typed languages that can express this constraint
-- example in Idris
-- data Vect : Nat -> Type -> Type where
--   Nil : Vect 0 a
--   (::) : a -> Vect n a -> Vect (S n) a
-- type Matrix a = Vect m (Vect n a)
-- type Row a = Vect n a
-- type Column a = Vect m a
-- type Square a = Matrix n a
-- type Grid a = Matrix 9 a
-- type Digit = Char
-- type Choices = [Digit]
-- digits = ['1' .. '9']
-- blank = '0'

-- example in Agda
-- data Matrix (m n : Nat) (a : Set) : Set where
--   mat : (rows : Vect m (Vect n a)) -> Matrix m n a
-- type Matrix a = Matrix m n a
-- type Row a = Vect n a
-- type Column a = Vect m a
-- type Square a = Matrix n n a
-- typesolveOptimation3 :: Grid -> [Grid]
solveOptimation3 :: Grid -> [Grid]
solveOptimation3 = search . choices

searchInitial :: Matrix [Digit] -> [Grid]
searchInitial cm
  | complete pm = [extract pm]
  | otherwise = concatMap search (expand1 pm)
  where
    pm = prune cm

-- fibonacci :: Int -> [Int]
-- fibonacci n = take n $ foldr (\_ (a : b : xs) -> (a + b) : a : b : xs) [1, 0] [1 ..] tMap search (expand1 pm)

type Row a = [a]

type Matrix a = [Row a]

type Grid = Matrix Digit

type Digit = Char

digits :: [Char]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')

validSo :: Grid -> Bool
-- valid suduko
-- In the context of Sudoku, a valid grid typically means that
-- each row, column, and 3x3 subgrid contains no duplicate numbers
-- (ignoring zeros or blanks, which represent unfilled cells)
validSo g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x : xs) = notElem x xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [] = [] -- Handle the empty list case
cols [xs] = [[x] | x <- xs]
cols (xs : xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroupS . ungroupS . map cols . groupS . map groupS

groupS :: [a] -> [[a]]
groupS [] = [] -- Handle the empty list case
groupS xs = take 3 xs : groupS (drop 3 xs)

ungroupS :: [[a]] -> [a]
ungroupS = concat

expand :: Matrix [Digit] -> [Grid]
-- expand :: Matrix [Digit] -> [Grid]: The expand function takes a matrix of lists of digits
-- and returns a list of all possible grids that can be formed by combining the digits in each cell.
-- exameple
-- input
-- [[1] [1 .. 9] [3] [4]]
-- output
-- [[1] [1] [3] [4]]
-- [[1] [2] [3] [4]]
-- [[1] [3] [3] [4]]
-- [[1] [4] [3] [4]]
-- [[1] [5] [3] [4]]
-- ..
-- ..
-- [[4] [1] [3] [1]]
-- ..
expand = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
-- inneficent implementation
-- cp (xs : xss) = [x : ys | x <- xs, ys <- cp xss]
cp (xs : xss) = [x : ys | x <- xs, ys <- cp yss] where yss = cp xss

choices :: Grid -> Matrix [Digit]
-- The choices function in Haskell, as defined in the provided code snippet, operates on a Sudoku grid. A Sudoku grid (Grid) is typically represented as a matrix (a list of lists) of digits (Digit). The choices function transforms this grid into a matrix where each cell contains a list of possible digits that could occupy that cell, based on the initial state of the grid.
-- Here's a breakdown of how choices works:
-- It applies a function to every cell in the grid using map (map choice'). This means it maps over every row, and within each row, it maps over every cell.
-- The choice' function checks if a cell is blank (typically represented by a zero or a specific blank character). If the cell is blank, it returns a list of all possible digits (digits) that could fill that cell. If the cell is not blank (meaning it already contains a digit), it returns a list containing only that digit.
-- input
-- 1 0 0 4
-- 0 0 3 0
-- 0 2 0 0
-- 4 0 0 2

-- output
-- [[1] [1,2,3,4] [1,2,3,4] [4]]
-- [[1,2,3,4] [1,2,3,4] [3] [1,2,3,4]]
-- [[1,2,3,4] [2] [1,2,3,4] [1,2,3,4]]
-- [[4] [1,2,3,4] [1,2,3,4] [2]]
choices = map (map choice')
  where
    choice' d = if blank d then digits else [d]

completions :: Grid -> [Grid]
completions = expand . choices

pruneBy :: ([Row [Digit]] -> [Row [Digit]]) -> [Row [Digit]] -> [Row [Digit]]
pruneBy f = f . map pruneRow . f

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
  where
    fixed = [d | [d] <- row]

remove :: [Digit] -> [Digit] -> [Digit]
remove _ [x] = [x]
remove ds xs = filter (`notElem` ds) xs

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

many :: (Eq a) => (a -> a) -> a -> a
many f x = if x == y then x else many f y where y = f x

-- The solve function definition in Haskell you've provided is a composition of several functions to solve a Sudoku puzzle. Let's break down what each part does and how they work together:
-- choices: This function is applied first to the input Grid. It likely transforms the grid into a matrix where each cell contains all possible digits that could fit there, based on the initial state of the Sudoku puzzle. If a cell is already filled in the puzzle, it will contain a singleton list with that digit. If a cell is empty, it will contain a list of all digits (1-9) that are potential candidates for that cell.
-- many prune: After choices generates all possible candidates for each cell, prune is applied repeatedly to the result until it no longer changes. The prune function itself likely reduces the list of possible digits for each cell by removing candidates that are no longer valid due to Sudoku rules (e.g., if a digit appears in the same row, column, or box, it can't appear in the same unit of another cell). The many function facilitates this by repeatedly applying prune until the grid stabilizes and no further pruning is possible.
-- expand: This function is applied to the pruned grid. expand likely takes the matrix of possible digits for each cell and generates all possible combinations of digits for the entire grid, based on the candidates available for each cell after pruning. Each combination represents a potential solution to the Sudoku puzzle.
-- filter validSo: Finally, the list of potential solutions generated by expand is filtered through validSo. This function checks each grid to see if it's a valid solution to the Sudoku puzzle. It likely verifies that the Sudoku rules are followed: each row, column, and 3x3 box must contain all digits from 1 to 9 exactly once. Only grids that pass this validation are kept.
-- The composition of these functions (filter validSo . expand . many prune . choices) forms a pipeline that transforms an initial Sudoku grid into a list of all possible solutions. The use of function composition (.) in Haskell allows for a concise and readable definition of this process.

solveOptimation2 :: Grid -> [Grid]
-- solve = filter validSo . completions inneficient
solveOptimation2 = filter validSo . expand . many prune . choices

-- lets keep optimizing

single :: [a] -> Bool
single [_] = True
single _ = False

-- we want to create a function expands
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 rows' = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    -- this implematation is inneficient
    -- (rows1, row : rows2) = break (any (not . single)) rows'
    -- (row1, cs : row2) = break (not . single) row
    (rows1, row : rows2) = case (rows1, row) of
      (rows'', _) -> break (any smallest) rows''
      (_, []) -> break (any smallest) rows'
    (row1, cs : row2) = break smallest row
    smallest cs' = length cs' == n
    n = minimum $ counts rows'
    counts = filter (/= 1) . map length . concat

expandRefactor1 :: Matrix [Digit] -> [Grid]
expandRefactor1 = concatMap expand . expand1

complete :: Matrix [Digit] -> Bool
complete = all (all single)

safe :: Matrix [Digit] -> Bool
safe cm =
  all ok (rows cm)
    && all ok (cols cm)
    && all ok (boxs cm)
  where
    ok row = nodups [x | [x] <- row]

extract :: Matrix [Digit] -> Grid
extract = map (map head)

solveOptimation4 :: Grid -> [Grid]
solveOptimation4 = search . choices

search :: Matrix [Digit] -> [Grid]
search cm
  | not (safe cm) = []
  | complete pm = [extract pm]
  | otherwise = concatMap search (expand1 pm)
  where
    pm = prune cm

-- chapter 6 notes
-- proofs
-- in our case using induction
-- base case
-- inductive step
-- we can use the same approach to prove the correctness of our programs
-- we can use induction to prove the correctness of our programs
-- we can use high-order functions to capture common patterns

-- exponential function
expBook6 :: (Num a, Eq a) => a -> a -> a
expBook6 x n
  | n == 0 = 1
  | n == 1 = x
  | otherwise = x * expBook6 x (n - 1)

-- how to test
-- exp x (m+n) = exp x m * exp x n
-- by induction over m and we need to prove the base case and the inductive step
-- case [] base case
-- exp x (0+n) = exp x 0 * exp x n
-- exp x 0 * exp x n = 1 * exp x n
-- 1 * exp x n = 1 * exp x n
-- exp x n = exp x n

-- There is induction o

-- The fusion law of foldr is a powerful optimization rule in functional programming, particularly in Haskell. It allows you to combine a foldr with a function that processes its result, potentially eliminating intermediate data structures and improving performance.

-- Fusion Law of foldr
-- The fusion law states that if you have a function h that can be expressed as h . foldr f z, then under certain conditions, you can fuse h directly into the foldr to avoid creating an intermediate list. The law can be written as:

-- [ h \circ \text{foldr} , f , z = \text{foldr} , g , e ]

-- where g and e are defined such that:

-- [ h , (f , x , y) = g , x , (h , y) ] [ h , z = e ]

-- Conditions for Fusion
-- For the fusion law to hold, the function h must distribute over the foldr operation. This means that h must be able to be applied to the result of the foldr in a way that allows it to be fused into the foldr itself.

-- Example
-- Consider the function map which applies a function f to each element of a list. The map function can be defined using foldr as follows:

-- Now, suppose we want to fuse map f with another foldr operation. For example, let's say we have:

-- Using the fusion law, we can fuse map (^2) into the foldr:

-- Identify h as map (^2).
-- Define g and e such that:
-- h (f x y) = g x (h y)
-- h z = e
-- For map (^2), we have:

-- h (x : xs) = (^2) x : map (^2) xs
-- h [] = []
-- So, we can rewrite sumSquares as:

-- This eliminates the intermediate list created by map (^2) and directly computes the sum of squares.

-- Summary
-- The fusion law of foldr allows you to combine a foldr with a function that processes its result, potentially eliminating intermediate data structures. The key is to express the function h in a way that it can be fused into the foldr, defining appropriate g and e functions. This optimization can lead to more efficient code by reducing the overhead of intermediate lists.

-- The primary difference between foldr (fold right) and foldl (fold left) in Haskell lies in how they traverse the list and combine the elements with the accumulator function. Here’s a detailed comparison:

-- foldr (fold right)
-- Traversal: foldr processes the list from right to left.
-- Function Application: The function is applied starting from the rightmost element and moving to the left.
-- Lazy Evaluation: foldr can work with infinite lists because it can produce results without necessarily traversing the entire list.
-- Signature: foldr :: (a -> b -> b) -> b -> [a] -> b
-- Example:
-- foldl (fold left)
-- Traversal: foldl processes the list from left to right.
-- Function Application: The function is applied starting from the leftmost element and moving to the right.
-- Strict Evaluation: foldl is strict in the accumulator, meaning it evaluates the accumulator as it traverses the list. This can lead to stack overflow with large lists.
-- Signature: foldl :: (b -> a -> b) -> b -> [a] -> b
-- Example:
-- Key Differences:
-- Order of Application:

-- foldr applies the function starting from the rightmost element.
-- foldl applies the function starting from the leftmost element.
-- Evaluation Strategy:

-- foldr can work with infinite lists due to its lazy nature.
-- foldl is strict and can lead to stack overflow with large lists.
-- Use Cases:

-- foldr is often used when constructing new lists or working with potentially infinite lists.
-- foldl is used when you need to accumulate results in a strict manner, such as summing a list of numbers.
-- Example Comparison:
-- Consider a function to concatenate a list of strings:

-- Using foldr:
-- Using foldl:
-- Both foldr and foldl produce the same result in this case, but the order of function application differs.

-- Summary
-- Use foldr when dealing with potentially infinite lists or when the function is naturally right-associative.
-- Use foldl when you need strict evaluation and are working with finite lists.

-- In Haskell, the scanl function is similar to foldl, but instead of returning just the final result, it returns a list of successive reduced values from the left. Essentially, scanl produces a list of all intermediate accumulator states.

-- Signature
-- Parameters
-- (b -> a -> b): A binary function that takes an accumulator and a list element and returns a new accumulator.
-- b: The initial accumulator value.
-- [a]: The input list.
-- Returns
-- [b]: A list of intermediate accumulator states, including the initial accumulator.
-- Example
-- Consider the following example where we use scanl to compute the running totals of a list of numbers:

-- Explanation
-- The initial accumulator is 0.
-- The first element 1 is added to the accumulator 0, resulting in 1.
-- The next element 2 is added to the new accumulator 1, resulting in 3.
-- The next element 3 is added to the new accumulator 3, resulting in 6.
-- The next element 4 is added to the new accumulator 6, resulting in 10.
-- The resulting list [0, 1, 3, 6, 10] includes the initial accumulator and all intermediate results.

-- Use Cases
-- Running Totals: As shown in the example, scanl can be used to compute running totals or other cumulative operations.
-- Intermediate States: When you need to keep track of all intermediate states of a computation, scanl is useful.
-- Visualization: Useful for visualizing the step-by-step transformation of an accumulator.
-- Comparison with foldl
-- foldl only returns the final accumulator value.
-- scanl returns a list of all intermediate accumulator values, including the initial value.
-- Example with Strings
-- Using scanl to concatenate a list of strings:

-- This shows the intermediate states of the concatenation process.

-- Summary
-- scanl is a powerful function in Haskell that provides insight into the intermediate states of a left fold operation, making it useful for debugging, visualization, and cumulative computations.

-- Fib

-- fibonacci :: Int -> [Int]
-- fibonacci n = take n $ foldr (\_ (a:b:xs) -> (a + b) : a : b : xs) [1, 0] [1..]

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [] : map (x :) (inits xs)

scanlBook :: (b -> a -> b) -> b -> [a] -> [b]
scanlBook f q xs =
  q
    : ( case xs of
          [] -> []
          x : xss -> scanlBook f (f q x) xss
      )

-- taking the mean

meanOwnImpl :: [Float] -> Float
-- solving first problem of typoe casting
meanOwnImpl xs = sum xs / fromIntegral (length xs)

-- one traverse
sumLen :: [Float] -> (Float, Int)
-- sumLen = foldr (\x (s, n) -> (s + x, n + 1)) (0, 0)
sumLen [] = (0, 0)
sumLen (x : xs) = (s + x, n + 1)
  where
    (s, n) = sumLen xs

-- but we know by now that fold can provide an optimal solution
sumLenFold :: [Float] -> (Float, Int)
sumLenFold = foldr (\x (s, n) -> (s + x, n + 1)) (0, 0)

sumLenBter :: [Float] -> (Float, Int)
sumLenBter = foldl' (\(s, n) x -> (s + x, n + 1)) (0, 0)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x : xs) = let z' = f z x in z' `seq` foldl' f z' xs

-- chapter 11 parser

newtype Parser a = Parser (String -> [(a, String)])

applyP :: Parser a -> String -> [(a, String)]
applyP (Parser p) = p

parseMine :: Parser a -> String -> a
parseMine p = fst . head . applyP p

-- Explanation
-- Functor Instance:

-- fmap f p = Parser (\inp -> [(f v, out) | (v, out) <- applyP p inp]): This defines how to apply a function f to the result of a Parser.
-- Applicative Instance:

-- pure v = Parser (\inp -> [(v, inp)]): This defines how to create a Parser that always returns the value v without consuming any input.
-- pf <*> px = Parser (\inp -> [(f v, out2) | (f, out1) <- applyP pf inp, (v, out2) <- applyP px out1]): This defines how to apply a Parser that produces a function (pf) to a Parser that produces a value (px).
-- Monad Instance:

-- p >>= f = Parser (\inp -> concat [applyP (f v) out | (v, out) <- applyP p inp]): This defines how to chain parsers together, where the result of the first parser (p) is used to determine the next parser (f).
-- By ensuring that the Applicative instance is defined, you satisfy the requirements for the Monad instance, and the error should be resolved.

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser (\inp -> [(f v, out) | (v, out) <- applyP p inp])

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\inp -> [(x, inp)])
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = Parser (\inp -> [(f v, out2) | (f, out1) <- applyP pf inp, (v, out2) <- applyP px out1])

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser (\inp -> concat [applyP (f v) out | (v, out) <- applyP p inp])

-- basic parsers
getc :: Parser Char
getc = Parser f
  where
    f [] = []
    f (c : cs) = [(c, cs)]

-- parse satifies character
satP :: (Char -> Bool) -> Parser Char
satP p = do
  c <- getc
  if p c then return c else failM

failM :: Parser a
failM = Parser (const [])

-- adding guard combinators

guardSatP :: (Char -> Bool) -> Parser Char
guardSatP p = do
  c <- getc
  guard (p c)
  return c

guard :: Bool -> Parser ()
guard True = return ()
guard False = failM

charM :: Char -> Parser ()
charM x = do _ <- guardSatP (== x); return ()

stringM :: String -> Parser ()
stringM [] = return ()
stringM (x : xs) = do charM x; stringM xs; return ()

lowerM :: Parser Char
lowerM = guardSatP isLower

digitM :: Parser Int
digitM = do
  c <- guardSatP isDigit
  return (digitToInt c)
  where
    digitToInt c = ord c - ord '0'
    ord = fromEnum

-- ghci> applyP (stringM "hell") "hello"
-- [((),"o")]

(<|>) :: Parser a -> Parser a -> Parser a
-- -- Assume these are defined somewhere
-- parserA :: Parser Char
-- parserA = ...

-- parserB :: Parser Char
-- parserB = ...

-- -- Using the <|> operator
-- combinedParser :: Parser Char
-- combinedParser = parserA <|> parserB
p <|> q = Parser f where f s = let ps = applyP p s in if null ps then applyP q s else ps

lowersM :: Parser String
lowersM =
  do
    c <- lowerM
    cs <- lowersM
    return (c : cs)
    <|> return ""

-- ghci> applyP lowers "Upper"
-- [("","Upper")]

-- ghci> applyP lowers "isUpper"
-- [("is","Upper")]

wrong :: Parser Int
wrong = addition <|> digitM

best :: Parser Int
best = digitM >>= rest

rest :: Int -> Parser Int
rest m = do
  charM '+'
  n <- digitM
  return (m + n)

addition :: Parser Int
addition = do
  x <- digitM
  charM '+'
  y <- digitM
  return (x + y)

manyP :: Parser a -> Parser [a]
manyP p =
  do
    x <- p
    xs <- manyP p
    return (x : xs)
    <|> noneP

noneP :: Parser [a]
noneP = return []

lowers :: Parser [Char]
lowers = manyP lowerM

space :: Parser ()
space = Control.Monad.void (manyP (guardSatP isSpace))

symbol :: String -> Parser ()
symbol xs = do
  space >> stringM xs >> space

token :: Parser a -> Parser a
token p = space >> p

some :: Parser a -> Parser [a]
some p = do
  x <- p
  xs <- manyP p
  return (x : xs)

optional :: Parser [a] -> Parser [a]
optional p = p <|> noneP

natural :: Parser Integer
natural = token nat
  where
    nat = do
      xs <- some digitM
      return (foldl (\n d -> 10 * n + toInteger d) 0 xs)
