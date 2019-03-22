{-# LANGUAGE FlexibleContexts #-}

import Text.Printf
import Data.List (maximumBy, groupBy)

-- https://www.hackerrank.com/challenges/convex-hull-fp/problem
-- perimeter of convex hull figure 

-- newtype Point a = Point (a, a) deriving Show
-- data Line a = Non | Line (Point a, Point a) deriving Show
-- newtype ConvexHull a = ConvexHull [Line a]
-- per :: (Ord a, Num a) => ConvexHull a -> Double
-- per (ConvexHull []) = 0
-- per (ConvexHull (_:[])) = 0
-- per (ConvexHull (_:_:[])) = 0
-- per (ConvexHull lines) = sum $ map length lines

-- return proportial distance betwin point P and line p1->2p
distanceToPoint :: (Ord a, Num a) => (a, a) -> (a, a) -> (a, a) -> a
distanceToPoint (x1, y1) (x2, y2) (px, py) = ((py - y1) * (x2 - x1)) - ((y2 - y1)*(px - x1))

-- returns the side of point P with respect of line p1->p2
--  0 -> P on line
--  1 -> P upper of line
-- -1 -> P lower of line
findSide :: (Ord a, Num a) => (a, a) -> (a, a) -> (a, a) -> Int
findSide pa pb p = (\x -> if x > 0 then 1 else if x < 0 then -1 else 0) $ distanceToPoint pa pb p

lSide ((xa, ya), (xb, yb)) = sqrt $ (\x -> fromInteger x :: Double) $(+) ((xb - xa)^2) ((yb - ya)^2)

lineDist pa pb p = abs $ distanceToPoint pa pb p
pairSides p1 p2 pp = (side (>0) pp , side (<0) pp)
    where
        side f = filter (f . (findSide p1 p2))

quickHull' :: (Num a, Ord a) => [(a,a)] -> ((a,a),(a,a)) -> [((a,a),(a,a))]
quickHull' [] (p1,p2)  = [(p1,p2)]
quickHull' pp (minX, maxX) = (quickHull' l (minX, nP)) ++ (quickHull' r (nP, maxX)) 
    where
        nP = maximumBy distance pp
        distance p1 p2 = compare (distanceTo p1) (distanceTo p2)
        distanceTo = distanceToPoint minX maxX
        -- ignore what left inside aria
        (l, _) = pairSides minX nP pp
        (_, r) = pairSides maxX nP pp

quickHull :: (Num a, Ord a) => [(a, a)] -> [((a, a), (a, a))]
quickHull pp 
    | length pp < 3 = error "Convex hull not possible"
    | otherwise = hull
        where 
        (minX, maxX) = (minimum pp, maximum pp)
        (upSide, downSide) = pairSides minX maxX pp
        hull = (quickHull' upSide (minX,maxX)) ++ (quickHull' downSide (maxX, minX))

perimeter :: [((Integer, Integer), (Integer, Integer))] -> Double
perimeter = sum . map lSide

solve :: [(Integer, Integer)] -> Double
solve = perimeter . quickHull


main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = map (\[x, y] -> (x, y)). map (map (read::String->Integer)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans

points = [(113,201),(911,749),(839,217),(293,144),(290,848),(350,150),(143,995),(311,262),(923,748),(599,691),(128,790),(611,723),(881,577),(446,988),(209,589),(977,285),(512,813),(875,788),(566,674),(788,872),(320,738),(743,446),(227,271),(617,470),(761,859),(860,918),(866,868),(746,640),(167,39),(824,768),(593,184),(248,831),(197,232),(224,13),(677,131),(554,31),(35,572),(485,367),(422,828),(689,657),(314,954),(863,753),(806,315),(953,551),(992,351),(212,436),(917,26),(719,948),(707,606),(947,203),(119,798),(791,919),(260,394),(950,991),(59,164),(5,341),(92,191),(338,504),(383,695),(476,888),(602,133),(68,80),(818,277),(713,617),(827,971),(533,671),(455,300),(29,682),(605,71),(8,555),(32,449),(545,843),(215,526),(857,237),(926,634),(539,889),(335,656),(443,431),(269,402),(770,190),(680,978),(494,344),(242,763),(317,560),(803,73),(20,604),(785,154),(380,96),(536,669),(395,251),(236,977),(437,818),(389,412),(356,435),(23,500),(725,597),(587,481),(368,630),(776,791),(560,249)]
points' = [(1, 1),(2, 5),(3, 3),(5, 3),(3, 2),(2, 2)]
