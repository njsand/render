module Main where

import Data.Char(isNumber, toLower)
import Data.Array.IO
import System.Environment(getArgs)
import System.Random
import Safe(atMay)
import qualified Data.Vector.Storable as V
import qualified Data.Word as W
import qualified Codec.Picture as P
import Raster

-- TODO
-- 1. write usage to stderr (not stdout) / set return code on bad invocation.

main :: IO ()
main = do
  args <- getArgs
  let params = checkArgs args
  case params of
    Just (Params List d numLines name) -> mainList d numLines name
    Just (Params Array d numLines name) -> mainArray d numLines name
    Nothing -> usage

data SequenceType = List | Array deriving (Read)

-- Arguments given to the program.
data Params = Params {mode :: SequenceType,
                      dims :: Dimensions,
                      numLines :: Int,
                      filename ::String}

-- Image dimensions
type Width = Int
type Height = Int
type Dimensions = (Width, Height)

checkArgs :: [String] -> Maybe Params
checkArgs xs = do
  mode <- atMay xs 0
  width <- atMay xs 1
  height <- atMay xs 2
  lines <- atMay xs 3
  filename <- atMay xs 4
  boolMaybe $ and $ (all isNumber) <$> [width, height, lines]
  seqType <- checkSeqType mode
  boolMaybe $ not $ null filename 
  Just $ Params seqType ((read width), (read height)) (read lines) filename

-- Turn a Bool into a Maybe Bool.  Useful for predicates that return False when
-- we want them to return Nothing.
boolMaybe :: Bool -> Maybe Bool
boolMaybe True = Just True
boolMaybe False = Nothing

-- Check that a string corresponds to one of the SequenceType values.
checkSeqType :: String -> Maybe SequenceType
checkSeqType s
    | map toLower s == "array" = Just Array
    | map toLower s == "list" = Just List
    | otherwise = Nothing

-- Write a useage message to stdout.
usage :: IO ()
usage = putStrLn "usage: render {array|list} width height number-of-lines output-filename"

-- Do everything using a list of lists as the frame buffer.  Probably going to
-- be pretty slow.
mainList :: Dimensions -> Int -> String -> IO ()
mainList dims@(w, h) numLines file = do
  lines <- sequence $ fmap randLine [dims | _ <- [1 .. numLines]]
  let img = foldl drawLine (pureBlackImg dims) lines
  writePng w h (imageToVector (concat img)) file

-- Do everything using a 2d array as the frame buffer.  We'd hope this was
-- faster than the list version.
mainArray :: Dimensions -> Int -> String -> IO ()
mainArray dims@(w, h) numLines file = do
  arr <- newArray ((0, 0), (h - 1, w - 1)) 0
  lines <- sequence $ fmap randLine [dims | _ <- [1 .. numLines]]
  drawLinesA arr lines
  elements <- getElems arr
  writePng w h (imageToVector elements) file
  -- writePbm $ rectangle elements w

-- Write an image to file NAME via the JuicyPixels lib.
writePng :: Int -> Int -> V.Vector W.Word8 -> String -> IO ()
writePng w h v name = P.savePngImage name img
           -- where img = P.ImageY8 $ P.Image 100 100 (V.replicate 30000 255)
           where img = P.ImageY8 $ P.Image w h v

-- Just turn a list of something like [Int] to a vector of bytes.  JuicyPixels
-- likes to work with vectors it seems.
imageToVector :: (Eq a, Num a) => [a] -> V.Vector W.Word8
imageToVector = V.fromList . (map toWord)
    where toWord x
              | x /= 0 = 255
              | otherwise = 0

-- An image of all zero pixels.
pureBlackImg :: Dimensions -> [[Int]]
pureBlackImg (w, h) = [[0 | _ <- [1..w]] | _ <- [1..h]]

-- Return an IO action that yields a random line within the provided dimensions.
randLine :: Dimensions -> IO Line
randLine (w, h) = do
  g0 <- newStdGen
  let (x1, g1) = randomR (0, w-1) g0
      (y1, g2) = randomR (0, h-1) g1
      (x2, g3) = randomR (0, w-1) g2
      (y2, g4) = randomR (0, h-1) g3
  return $ makeLine (x1, y1) (x2, y2)

         
