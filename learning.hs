module Learning where

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs

--replaceNthRow :: Double -> Double -> a -> [[a]] -> [[a]]
--replaceNthRow _ _ _ [[]] = [[]]
--replaceNthRow row col newVal ((l:ls):xs)
-- | row == 0 = replaceNth col newVal ls
-- | otherwise = l:replaceNthRow (row-1) col newVal xs

changeElem :: Int -> Int -> a -> [[a]] -> [[a]]
-- empty list case
changeElem _ _ _ [] = []

-- have arrived at the element to change`
changeElem 0 0 x ((y:ys):ls) = (x:ys):ls

-- reduce the column until we find the element to change
changeElem 0 col x ((y:ys):ls) = [[y]] ++ changeElem 0 (col-1) x (ys:ls)

changeElem row col x xs =
    let row_to_replace_in = xs !! row
        modified_row = replaceNth col x row_to_replace_in
    in replaceNth row modified_row xs

getVal :: Int -> Int -> [[a]] -> a
getVal row col ls = (ls !! row) !! col
