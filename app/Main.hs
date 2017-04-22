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
import System.Random

-- import Codec.Picture

-- TODO
-- 1. write usage to stderr / set return code?
-- 2. Write out something easier to view than pbm.
-- 3. Use the standard library version of myAtMay

main :: IO ()
main = do
  args <- getArgs
  let params = checkArgs args
  case params of
    Just (Params List d numLines) -> mainList d numLines
    Just (Params Array d numLines) -> mainArray d numLines
    Nothing -> usage

data SequenceType = List | Array deriving (Read)                                                                                                                                                                                              
data Params = Params {mode :: SequenceType, dims :: Dimensions, numLines :: Int}

-- Image dimensions
type Width = Int
type Height = Int
type Dimensions = (Width, Height)

checkArgs :: [String] -> Maybe Params
checkArgs xs = do
  mode <- myAtMay xs 0
  width <- myAtMay xs 1
  height <- myAtMay xs 2
  lines <- myAtMay xs 3
  boolMaybe $ and $ (all isNumber) <$> [width, height, lines]
  seqType <- checkSeqType mode
  Just $ Params seqType ((read width), (read height)) (read lines)

boolMaybe :: Bool -> Maybe Bool
boolMaybe True = Just True
boolMaybe False = Nothing

checkSeqType :: String -> Maybe SequenceType
checkSeqType s
    | map toLower s == "array" = Just Array
    | map toLower s == "list" = Just List
    | otherwise = Nothing

usage :: IO ()
usage = putStrLn "usage: render Array|List width height number-of-lines"

myAtMay :: [a] -> Int -> Maybe a
myAtMay [] _ = Nothing
myAtMay (x:xs) 0 = Just x
myAtMay (_:xs) n = myAtMay xs (n - 1)

-- Do everything using a list of lists as the frame buffer.  Probably going to
-- be pretty slow.
mainList :: Dimensions -> Int -> IO ()
mainList dims@(w, h) numLines = do
  lines <- sequence $ fmap randLine [dims | _ <- [1 .. numLines]]
  writePbm $ foldl drawLine (pureBlackImg dims) lines

-- Do everything using a 2d array as the frame buffer.  We'd hope this was
-- faster than the list version.
mainArray :: Dimensions -> Int -> IO ()
mainArray dims@(w, h) numLines = do
  arr <- newArray ((0, 0), (h - 1, w - 1)) 0
  lines <- sequence $ fmap randLine [dims | _ <- [1 .. numLines]]
  drawLinesA arr lines
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

-- Return an IO action that yields a random line within the provided dimensions.
randLine :: Dimensions -> IO Line
randLine (w, h) = do
  g0 <- getStdGen
  let (x1, g1) = randomR (0, w-1) g0
      (y1, g2) = randomR (0, h-1) g1
      (x2, g3) = randomR (0, w-1) g2
      (y2, g4) = randomR (0, h-1) g3
  setStdGen g4
  return $ makeLine (x1, y1) (x2, y2)
