-- Rasterize a bunch of random lines to a pixmap using either an IOUArray or a list
-- of lists as the pixmap.
--
-- This was written mostly to begin to explore answers to the question "How do I
-- do arrays in Haskell?"
--
-- Using linked lists to implement a random access 2d array (pretty much what a
-- pixmap is?) is pretty much the wrong data structure for the job.  The right
-- one is an array.  So we implement both in this program to see just how much
-- faster the array version is.
--
-- The width and height of the pixmap, and the number of lines to draw are
-- supplied on the command line.  As mentioned above, lines are created
-- randomly, so the program output will be different from run to run.
--
-- Usage: render.exe list|array width height number-of-lines

module Main where

import Pbm
import Raster
import Data.Char
import Data.Array.IO
import System.Environment

-- import Codec.Picture

-- todo
-- 1. Allow width and height on the command line - thread these through code.
-- 2. Make it do N random lines instead of a grid - N on command line
-- 3. write usage to stderr / set return code?
-- 4. Write out something easier to view than pbm.

main :: IO ()
main = do
  args <- getArgs
  let params = checkArgs args
  case params of
    Just (Params List d numLines) -> mainList d numLines
    Just (Params Array d numLines) -> mainArray d numLines
    Nothing -> usage

data Sequence = List | Array deriving (Read)                                                                                                                                                                                              
data Params = Params {mode :: Sequence, dims :: Dimensions, numLines :: Int}

-- Image dimensions
type Dimensions = (Int, Int)

-- TODO: Make this fail for bad program args.
checkArgs :: [String] -> Maybe Params
checkArgs xs = Just $ Params (read (xs !! 0))
                             ((read (xs !! 1)), (read (xs !! 2)))
                             (read (xs !! 3))
  
usage :: IO ()
usage = putStrLn "render.exe width height num-lines"

myAtMay :: [a] -> Int -> Maybe a
myAtMay [] _ = Nothing
myAtMay (x:xs) 0 = Just x
myAtMay (_:xs) n = myAtMay xs (n - 1)

-- Do everything using a list of lists as the frame buffer.  Probably going to
-- be pretty slow.
mainList :: Dimensions -> Int -> IO ()
mainList dims@(w, h) numLines = writePbm img2
       where img1 = foldl drawLine (pureBlackImg dims) (hLines dims numLines)
             img2 = foldl drawLine img1 (vLines dims numLines)

-- Do everything using a 2d array as the frame buffer.  We'd hope this was
-- faster than the list version.
mainArray :: Dimensions -> Int -> IO ()
mainArray dims@(w, h) numLines = do
  arr <- newArray ((0, 0), (h - 1, w - 1)) 0
  drawLinesA arr (hLines dims numLines)
  drawLinesA arr (vLines dims numLines)
  elements <- getElems arr
  writePbm $ rectangle elements w

-- Turn a list into a list of lists, each one of length N.  (The final list
-- will be shorter than N if (length XS) is not a multiple of N.)
rectangle :: [a] -> Int -> [[a]]
rectangle [] _ = []
rectangle xs n = take n xs : rectangle (drop n xs) n

-- main = writePbm $ drawLine (makeLine (0,0) (width - 1, height - 1)) pureBlackImg
-- main = imageCreator "foo.png"

-- Juicy pixels lib.
-- imageCreator :: String -> IO ()
-- imageCreator path = writePng path $ generateImage pixelRenderer 250 300
--    where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

-- An image of all zero pixels.
pureBlackImg :: Dimensions -> [[Int]]
pureBlackImg (w, h) = [[0 | _ <- [1..w]] | _ <- [1..h]]

-- Alternative way to write this lol.
-- allBlack = replicate height $ replicate width 0

-- horizontal lines!
hLines :: Dimensions -> Int -> [Line]
hLines (w, h) numLines = zipWith makeLine [(0, y) | y <- ys]
                                          [(w - 1, y) | y <- ys]
    where step = h `div` numLines
          ys = [0 , step .. h - 1]
          
-- vertical lines!
vLines :: Dimensions -> Int -> [Line]
vLines (w, h) numLines = zipWith makeLine [(x, 0) | x <- xs]
                                          [(x, h - 1) | x <- xs]
    where step = w `div` numLines
          xs = [0,step .. w - 1]
