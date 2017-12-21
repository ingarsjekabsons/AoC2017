module A3_spiral_memory where
  
{-

You come across an experimental new kind of memory stored on an infinite two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

For example:

    Data from square 1 is carried 0 steps, since it's at the access port.
    Data from square 12 is carried 3 steps, such as: down, left, left.
    Data from square 23 is carried only 2 steps: up twice.
    Data from square 1024 must be carried 31 steps.

How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?


-}

import Data.List
import Data.Maybe -- fromJust

-- generate starting "straight" edges for each perimeter
-- they are - 2,11,28,53 etc.
genStraightEdges accum (x:xs) = (accum + (8*x)+1):genStraightEdges (accum + (8* x)+1) xs
genStraightEdges accum [] = accum:[]

straightEdges = genStraightEdges 1 [0,1..]


-- +2 since I can't figure out edge generator algorithm that deals
-- with first two perimeters.
--   well.. first one (1) would be easy-peasy, but second is special case
--   since it's the only parameter where first value starts at the "straight" edge :(
getPerimeterNo :: [Int] -> Int -> Int
getPerimeterNo x i = (fromJust $ elemIndex i x) + 2


calcEdgeValue :: [Int] -> Int -> Int -> Int
calcEdgeValue firstEdges firstEdgeValue edgeNumber = 
    firstEdgeValue + (2*((getPerimeterNo firstEdges firstEdgeValue)-1)*edgeNumber)



allEdges = [calcEdgeValue straightEdges x y | x <- straightEdges, y <- [0..3]]


getEdgesBetween :: [Int] -> Int -> (Int,Int)
getEdgesBetween (x:y:xs) i
                | (x <= i && y >= i ) = (x,y)
                | otherwise = getEdgesBetween (y:xs) i
                
distanceToClosestEdge :: [Int] -> Int -> Int
distanceToClosestEdge (x:y:xs) i
                | (x <= i && y >= i) = min (abs(x-i)) (abs(y-i))
                | otherwise = distanceToClosestEdge (y:xs) i
                

closest :: Int -> (Int, Int) -> Int
closest i (x,y) = if ( abs (x - i) < abs (y - i)) then x else y

closestEdge :: [Int] -> Int -> Int
closestEdge x e = closest e $ getEdgesBetween x e

distanceFromEdge :: [Int] -> Int -> Int
distanceFromEdge x e = (div (fromJust $ elemIndex e x) 4) + 1

solution :: [Int] -> Int -> Int
solution x cell = distanceToClosestEdge x cell + distanceFromEdge x (closestEdge x cell)

-- solution allEdges 45 ==> 4
-- solution allEdges 12 ==> 3
-- solution allEdges 65 ==> 8

