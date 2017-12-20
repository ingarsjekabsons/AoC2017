module A2_spreadsheet where
 
{-

The spreadsheet consists of rows of apparently-random numbers. To make sure the recovery process is on the right track, they need you to calculate the spreadsheet's checksum. For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.

For example, given the following spreadsheet:

5 1 9 5
7 5 3
2 4 6 8

    The first row's largest and smallest values are 9 and 1, and their difference is 8.
    The second row's largest and smallest values are 7 and 3, and their difference is 4.
    The third row's difference is 6.

In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.

-}
 
seekMinMax :: [Int] -> (Int, Int)
seekMinMax [] = (maxBound::Int, minBound::Int)
seekMinMax (x:xs) = (min x minRest, max x maxRest)
    where (minRest, maxRest) = seekMinMax xs

minMaxDiff :: (Int,Int) -> Int
minMaxDiff (min, max) = max - min

solution :: [[Int]] -> Int
solution (x:xs) = (minMaxDiff $ seekMinMax x) + solution xs
solution [] = 0


{-

b = [[9,2,3],[3,8,2]]
solution b -- gives 13 -- (9 - 3) + (8 - 3)

-}
