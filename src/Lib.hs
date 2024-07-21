{-# LANGUAGE InstanceSigs #-}

module Lib where

import Data.Char (isAlpha, toLower, toUpper)
import Data.List (group, sort, sortBy)
import Data.Ord
import Text.ParserCombinators.ReadP (choice)

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
      a ^ 3 + b ^ 3 == c ^ 3 + d ^ 3
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
-- type Grid a = Matrix 9 9 a
-- type Digit = Char
-- type Choices = [Digit]
-- digits = ['1' .. '9']
-- blank = '0'
-- limitations on type constraints in haskell cannot be annotated
-- however, they should all be the same length

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
cp :: [[a]] -> [[a]]
cp [] = [[]]
-- inneficent implementation
-- cp (xs : xss) = [x : ys | x <- xs, ys <- cp xss]
cp (xs : xss) = [x : ys | x <- xs, ys <- cp yss] where yss = cp xss
expand = cp . map cp

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

-- solve :: Grid -> [Grid]: The solve function takes a Grid as input
-- and returns a list of Grids. The input is the initial state of the Sudoku puzzle,
-- and the output is a list of all possible solutions to the puzzle.
solve :: Grid -> [Grid]
solve = filter validSo . completions

-- we want to prune such that
-- filter validSo . completions = filter validSo . completions . prune

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