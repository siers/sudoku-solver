module Sudoku where

import Data.List.Split
import Data.Tuple
import Data.List
import Control.Applicative

type Line   = [Int]
type Board  = [Line]

board :: String -> Board
board = splitEvery 9 . map read . words

horizontal :: Board -> Int -> Line
horizontal = (!!)

vertical :: Board -> Int -> Line
vertical = horizontal . transpose

sqLine segment = map (+segment*3) [0..2]

square :: Board -> (Int, Int) -> Line
square b (x, y) = map ((!!) . (b!!)) (sqLine y) <*> (sqLine x)

squareFind i = swap $ divMod i 3
