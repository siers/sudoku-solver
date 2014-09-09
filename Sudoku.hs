module Sudoku where

import Data.List.Split
import Data.Tuple
import Data.List
import Control.Applicative
import Control.Arrow
import Control.Lens

type Pos      = (Int, Int)
type Point a  = (Pos, a)
type Points a = [Point a] -- Numbers to be implemented into board.

-- Board should be a matrix, but not obligatory a square one.
type Line a   = [a]
type Board a  = [Line a]

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

poses :: Board Pos
poses = map (flip map [0..8] . (,)) [0..8]

-- Generic Transformers
mapB :: (a -> b) -> Board a -> Board b
mapB = map . map


showB :: Show a => Board a -> String
showB = spacify "\n" . map (spacify " ") . mapB show
    where spacify s = foldr1 (++) . intersperse s

zipB :: [[a]] -> [[b]] -> [[(a, b)]]
zipB = zipWith zip

-- Line getters
horizontal :: Board a -> Pos -> Line a
horizontal b (h, _) = b !! h

vertical :: Board a -> Pos -> Line a
vertical = (. swap) . horizontal . transpose

square :: Board a -> Pos -> Line a
square b (x, y) = map ((!!) . (b!!)) (sqLine $ div x 3) <*> (sqLine $ div y 3)
    where sqLine segment = map (+segment*3) [0..2]

-- Homomorphisms
horizontals :: Board a -> Board a
horizontals = id

verticals :: Board a -> Board a
verticals = transpose

squares :: Board a -> Board a
squares b = withBoard b square toPos
    where
        toPos = ((*3) *** (*3)) . swap . flip divMod 3
        withBoard b f argf = map (f b . argf) [0..8]

-- Point related stuff.
cells :: Board a -> Board (Point a)
cells = zipB poses

implement :: Board a -> Point a -> Board a
implement b ((x, y), v) = b & ix x . ix y .~ v

on :: Board a -> Pos -> a
on b (x, y) = b !! x !! y

-- Slots and data for solving.
unify :: SSlots3 -> SSlots -- Board FieldsTakenIn3 -> Board FieldsNotTaken
unify = mapB $ ([1..9] \\) . concat

streamMap :: Board a -> Slots3 a
streamMap b = mapB choices poses
    where
        choices pos = map (($ (b, pos)) . uncurry) [horizontal, vertical, square]

slots :: SBoard -> SSlots
slots = unify . streamMap

streams :: Board a -> Line (Board a)
streams b = map ($ b) [horizontals, verticals, squares]
