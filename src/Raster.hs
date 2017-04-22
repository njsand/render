-- Provides code to rasterize shapes into a pixmap.  (Currently only lines.)

module Raster
    ( makeLine,
      drawLine,
      linePoints,
      drawLinesA,
      Line() -- Not exporting any ctors here!
    ) where

import Data.Array.IO
import Data.Tuple

-- Lines are constructed with the pair with the low x-cordinate pair in the
-- first slot, so we can always assume this about instances.
type Point = (Int, Int)

-- A line from point a to point b.
data Line = Line Point Point deriving (Show, Eq)

-- Constructs a Line, ensuring the points are sorted by the x co-ordinate
-- (smallest x co-ord is first.)
makeLine :: Point -> Point -> Line
makeLine p1@(x1, y1) p2@(x2, y2)
    | x1 > x2 = Line p2 p1
    | otherwise = Line p1 p2

type Image = [[Int]]

-- Return a list consisting of the points along line L.  IOW, all the pixels
-- that would make up the line on an image.
linePoints :: Line -> [Point]
linePoints (Line (x1, y1) (x2, y2))
    | x2 - x1 == 0 = zip (repeat x1) [min y1 y2 .. max y1 y2] -- vertical line
    | m > 1.0      = zip (steps x1 $ 1.0 / m) [y1..y2]
    | m >= 0       = zip [x1..x2] (steps y1 m)
    | m >= -1.0    = zip [x1..x2] (steps y1 m)
    -- The slope is less than -1.0 here
    | otherwise    = zip (steps x1 $ (-1.0) / m) [y1, y1 - 1 .. y2]
    where m = fromIntegral (y2 - y1) / fromIntegral (x2 - x1)

-- Reflect a point about the X axis (negate the Y co-ord.)
reflectXaxis :: Point -> Point
reflectXaxis (x1, y1) = (x1, -y1)

-- Return the larger y co-ordinate of two points.
maxY :: Point -> Point -> Int
maxY (x1, y1) (x2, y2) = max y1 y2

-- maxY = max `on` fst

-- An infinite list.
steps :: Int -> Double -> [Int]
steps start m = map round [dblStart, dblStart + m ..]
    where dblStart = fromIntegral start

-- "Set" a particular value in a list of lists.
set2 :: [[a]] -> (Int, Int) -> a -> [[a]]
set2 [] _ _ = []
set2 (x:xs) (0, y) v = set x y v : xs
set2 (x:xs) (n, y) v = x : set2 xs (n - 1, y) v

-- "Set" the element at index N.
set :: [a] -> Int -> a -> [a]
set [] _ _ = []
set (x:xs) 0 y = y:xs
set (x:xs) n y = x : set xs (n - 1) y

-- Here for fun - currently unused.
get :: [[a]] -> Int -> Int -> a
get xs i j = xs !! i !! j

-- Plot the points of the line in an image represented as an [[Int]].  (This
-- going to be slow.)
drawLine :: Image -> Line -> Image
drawLine image line = let points = linePoints line in
    drawPoints image points

drawPoints :: Image -> [Point] -> Image
drawPoints img [] = img
drawPoints img ((x,y):ps) = drawPoints (set2 img (y, x) 1) ps

-- Return an IO action that draws lines into an array.
drawLinesA :: (IOUArray (Int, Int) Int) -> [Line] -> IO (IOUArray (Int, Int) Int)
drawLinesA arr [] = return arr
drawLinesA arr (line:ls) = do
  let points = linePoints line
  mapM_ (flip (writeArray arr) $ 1) (map swap points)
  drawLinesA arr ls

-- Just some test bullshit.
test :: IO ()
test = do
  arr <- newArray ((0,0), (9,9)) 42 :: IO (IOUArray (Int, Int) Int)
  writeArray arr (0, 0) 43
  a <- readArray arr (0, 0)
  print a
        
