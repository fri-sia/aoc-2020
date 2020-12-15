module Main where

import qualified Data.Map.Strict as M

part1 :: M.Map Int Int -> Int -> Int -> Int
part1 m age prevNumber =
  case age of
    30000000 -> prevNumber
    _ -> case M.lookup prevNumber m of
      Nothing -> part1 (M.insert prevNumber age m) (1 + age) 0
      Just(n) -> let prev = age - n
                     map = M.insert prevNumber age m
                 in part1 map (1 + age) prev

main :: IO ()
main = do
  let startMap = M.fromList [(1,1), (20, 2), (8, 3), (12, 4), (0,5)]
  let n = part1 startMap 6 14
  print n
  return ()
