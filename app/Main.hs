module Main (main) where

import Lib

-- ghci> cwords 15 "/Users/scala/mine/haskell/thinking-func-cabal/thinking-func/app/Gutember.html
-- "
-- the: 34257
--  and: 21392
--  to: 16502
--  of: 14912
--  </p>: 11393
--  <p>: 11244
--  a: 10383
--  he: 9284
--  in: 8733
--  his: 7920
--  that: 7403
--  was: 7203
--  with: 5647
--  had: 5334

cwords :: Int -> FilePath -> IO ()
cwords n infile = do
  text <- readFile infile
  putStrLn (commonWord1s n text)
  putStrLn "Done"

main :: IO ()
main = someFunc
