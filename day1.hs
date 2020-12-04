

import Control.Applicative
import qualified System.IO.Strict as S

numbers :: IO [Int]
numbers = do
    fileContents <- S.readFile "./input/day1.input"
    return $ map read . lines $ fileContents

applicativeSum :: [Int] -> [Int] -> [Int]
applicativeSum xs ys = (+) <$> xs <*> ys

findCombinations :: Int -> Int -> [Int] -> [([Int], Int, Int)]
findCombinations target n scalars = filtered
    where groups = foldr (\x y -> (:) <$> x <*> y)  [[]] (take n . repeat $ scalars)
          groups' = map (\ls -> (ls, sum ls, product ls)) groups
          filtered = filter (\(_,s,_) -> s == target) groups'
