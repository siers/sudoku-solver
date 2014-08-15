module Solver where

import Data.Maybe
import Data.List
import Data.List.Split
import Control.Arrow
import Sudoku

-- Finds a number at pos to put on the board determinstically from the possibilities.
{-
trivialities :: (Slots Int -> Slots Int) -> Slots Int -> [Target Int]
trivialities inverse morphed = positions . inverse . concatMap targets . align $ morphed
    where
        align                      = zip slots . cells
        targets (slots, cellline)  = filter (intersect slots . snd) cellline
        positions                  = undefined
-}

frequency :: Ord a => Line a -> Line (Int, a)
frequency = map (length &&& head) . group . sort

-- Expects that the Board is a Line of possibilities within same restriction group.
uniq :: Ord a => Board a -> Line a
uniq = map snd . filter ((==1) . fst) . frequency . concat

-- Ord type restriction could is not needed for functionality, just for laziness.
trivialities :: Ord a => LSlots a -> [Point a]
trivialities = concatMap (targets) . align
    where
        align           = zip =<< simples
        simples         = map (uniq . map snd)
        sample          = find (not . null . snd) . return
        allowed uniques = map (second (intersect uniques))
        targets         = catMaybes . map (fmap (second head) . sample) . uncurry allowed
        -- targets uniques = map (second head) . filter (not null . snd) . map (second (intersect uniques))
        --targets (uniques, line)  = map (\(pos, slots) -> (pos, head $ intersect uniques slots)) line
        -- targets uniques = map . second . (head . intersect)

{-
trivialized b = concatMap (trivialities $ slots b) . flip zip inversions $ restrictions
    where
        inversions   = [id, verticals, squares . squares]
        restrictions = mapB uniques . streams . slots $ b
-}

trivialized :: SBoard -> [Point Int]
trivialized b = concatMap trivialities . streams $ possibs
    where
        possibs = cells . unify . takens $ b

trivializer :: SBoard -> SBoard
trivializer b =
    if length (trivialized b) /= 0
    then implement b . head . trivialized $ b
    else b

trivialize :: SBoard -> SBoard
trivialize = converge trivializer

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

convergerate :: Eq a => (a -> a) -> a -> [a]
convergerate f i = takeWhile (/= (converge f i)) $ iterate f i

solve = convergerate trivialize
