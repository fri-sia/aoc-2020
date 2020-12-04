

chart :: IO [String]
chart = do
    fileContents <- readFile "./input/day3.input"
    return . map (concat . repeat)  . lines $ fileContents

cycleN :: Int -> [a] -> [a]
cycleN n [] = []
cycleN n (x:xs) = x : (cycleN n . drop (n-1) $ xs)

countTrees :: (Int, Int) -> [String] -> Integer
countTrees (dx, dy) chart = fromIntegral . length $ onlyTrees
    where dyChart = cycleN dy chart
          takes = ((\s -> take s . repeat $ (drop dx)) <$> [0..])
          takes' = map (foldr (.) id) takes
          dxChart = zipWith ($) takes' dyChart
          onlyTrees = filter (== '#') (map head dxChart)

slopes :: [(Int, Int)]
slopes =
    [ (1, 1)
    , (3, 1)
    , (5, 1)
    , (7, 1)
    , (1, 2) ]

slopesProduct :: [String] -> Integer
slopesProduct chart = product . map (flip countTrees chart) $ slopes
