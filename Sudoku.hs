module Sudoku where

import Data.List.Split
import Data.Tuple
import Data.List
import Control.Applicative
import Control.Arrow

type Line a   = [a]
type Board a  = [Line a]
type Slots a  = Board (Line a)  -- slots of unified h/v/s
type Slots3 a = Board (Board a) -- slots of h/v/s

type SLine    = Line Int
type SBoard   = Board Int
type SSlots   = Slots Int  -- slots of unified h/v/s
type SSlots3  = Slots3 Int -- slots of h/v/s
type Pos      = (Int, Int)

-- Constructors
board :: String -> SBoard
board = chunksOf 9 . take 81 . (++ repeat 0) . map read . words

boardOf :: a -> Board a
boardOf = flip mapB (board "") . const

frees :: SSlots
frees = tnr . tnr $ [1..9]
    where tnr = take 9 . repeat

poses :: Board Pos
poses = map (flip map [0..8] . (,)) [0..8]

-- Generic Transformers
mapB :: (a -> b) -> Board a -> Board b
mapB = map . map

withBoard :: Board a -> (Board a -> Pos -> Line b) -> (Int -> Pos) -> Board b
withBoard board mfyer argifyer = map (mfyer board . argifyer) [0..8]

showB :: Show a => Board a -> String
showB = spacify "\n" . map (spacify " ") . mapB show
    where spacify s = foldr1 (++) . intersperse s

zipB :: [[a]] -> [[b]] -> [[(a, b)]]
zipB = zipWith zip

-- Accessors
horizontal :: Board a -> Pos -> Line a
horizontal b (h, _) = b !! h

vertical :: Board a -> Pos -> Line a
vertical = (. swap) . horizontal . transpose

square :: Board a -> Pos -> Line a
square b (x, y) = map ((!!) . (b!!)) (sqLine $ div x 3) <*> (sqLine $ div y 3)
    where sqLine segment = map (+segment*3) [0..2]

-- Transformers
horizontals :: Board a -> Board a
horizontals b = withBoard b horizontal (flip (,) 0)

-- Transformers
verticals :: Board a -> Board a
verticals b = withBoard b horizontal ((,) 0)

squares :: Board a -> Board a
squares b = withBoard b square toPos
    where toPos = ((*3) *** (*3)) . swap . flip divMod 3

cells :: Board a -> Board (Pos, a)
cells = zipB poses

choices :: Board a -> Pos -> Board a
choices b pos = map (($pos) . ($b)) [horizontal, vertical, square]

unify :: SSlots3 -> SSlots
unify = mapB $ ([1..9] \\) . concat

takens :: Board a -> Slots3 a
takens b = mapB (choices b) $ poses
