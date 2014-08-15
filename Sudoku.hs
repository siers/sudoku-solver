module Sudoku where

import Data.List.Split
import Data.Tuple
import Data.List
import Control.Applicative
import Control.Arrow
import Control.Lens

type Pos      = (Int, Int)
type Point a = (Pos, a)

-- Board should be a matrix, but not obligatory a square one.
type Line a   = [a]
type Board a  = [Line a]

type Points a = [Point a] -- Numbers to be implemented into board.

type Slots a  = Board (Line a)  -- slots of unified h/v/s
type Slots3 a = Board (Board a) -- slots of h/v/s

-- L = Lattice
type LSlots a = Board (Point (Line a))

type SLine    = Line Int
type SBoard   = Board Int
type SSlots   = Slots Int
type SSlots3  = Slots3 Int
type SLSlots  = LSlots Int

-- Constructors
board :: [Int] -> SBoard
board = boardFrom 0

boardFrom :: a -> [a] -> Board a
boardFrom filling = chunksOf 9 . take 81 . (++ repeat filling)

boardS :: String -> SBoard
boardS = board . map read . words

boardOf :: a -> Board a
boardOf = flip mapB (board []) . const

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

-- Transformers
horizontal :: Board a -> Pos -> Line a
horizontal b (h, _) = b !! h

vertical :: Board a -> Pos -> Line a
vertical = (. swap) . horizontal . transpose

square :: Board a -> Pos -> Line a
square b (x, y) = map ((!!) . (b!!)) (sqLine $ div x 3) <*> (sqLine $ div y 3)
    where sqLine segment = map (+segment*3) [0..2]

horizontals :: Board a -> Board a
horizontals = id

verticals :: Board a -> Board a
verticals = transpose

squares :: Board a -> Board a
squares b = withBoard b square toPos
    where toPos = ((*3) *** (*3)) . swap . flip divMod 3

implement :: Board a -> Point a -> Board a
implement b ((x, y), v) = b & ix x . ix y .~ v

-- squares . squares . squares == id
-- verticals . verticals == id
-- iterate (Board a -> Board a) (Board a) !! 6 == id

cells :: Board a -> Board (Point a)
cells = zipB poses

-- Board (n x n) a -> Pos -> Board (n x 3) a
choices :: Board a -> Pos -> Board a
choices b pos = map (($pos) . ($b)) [horizontal, vertical, square]

unify :: SSlots3 -> SSlots
unify = mapB $ ([1..9] \\) . concat

takens :: Board a -> Slots3 a
takens b = mapB (choices b) $ poses

slots :: SBoard -> SSlots
slots = unify . takens

streams :: Board a -> Line (Board a)
streams b = map ($ b) [horizontals, verticals, squares]
