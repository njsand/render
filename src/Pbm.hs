module Pbm
    ( writePbm
    ) where
    
-- Not sure if the height or width is the first field.

-- Write an image to stdout in PBM format.
writePbm :: [[Int]] -> IO ()
writePbm pixels = do
  putStrLn "P1"
  putStrLn "# This file was written by Pbm.hs, yeah!"
  putStr (show $ length $ head $ pixels) -- height
  putStr " "
  putStrLn (show $ length $ pixels)        -- width
  writeRows pixels

-- Write each image row on its own line.
writeRows :: [[Int]] -> IO ()
writeRows [] = return ()
writeRows (r:rs) = do
  mapM_ (putStr . ((++ " ") . show)) r
  putStrLn ""
  writeRows rs
