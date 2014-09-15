module Solver where

import Data.Maybe
import Data.List
import Data.List.Split
import Control.Arrow
import Control.Monad
import Sudoku

frequency :: Ord a => Line a -> Line (Int, a)
frequency = map (length &&& head) . group . sort

-- Expects that the Board is a Line of possibilities within same restriction group.
uniq :: Ord a => Int -> Board a -> Line a
uniq length = map snd . filter ((== length) . fst) . frequency . concat

-- Ord type restriction could is not needed for functionality, just for laziness.
-- Checks for unique numbers in lines of one of h/v/s board, retrieves them as Target Int.
choices :: Ord a => Int -> LSlots a -> [Point a]
choices length = concatMap targets . align
    where
        align           = zip =<< simples
        simples         = map (uniq length . map snd)
        sample          = find (not . null . snd) . return
        allowed uniques = map (second (intersect uniques))
        targets         = catMaybes . map (fmap (second head) . sample) . uncurry allowed

deduceChoices :: Int -> SBoard -> [[Point Int]]
deduceChoices count = return . nub . concatMap (choices count) . streams . cells . slots

deductions :: MonadPlus m => SBoard -> m SPoint
deductions = validity =<< body
    where
        logic   = msum . fmap return
        body    = logic . head . concat . flip map [1..9] . flip deduceChoices
        invalid = any null . concat
        validity choices board =
            if invalid . slots $ board
            then mzero
            else choices

trivialized :: SBoard -> [Point Int]
trivialized = deductions

logTrivialize :: ([Point Int], SBoard) -> ([Point Int], SBoard)
logTrivialize (_, b) =
    if length (trivialized b) /= 0
    then (trivialized $ b, implement b . head . trivialized $ b)
    else ([], b)

trivialize :: SBoard -> SBoard
trivialize = snd . curry logTrivialize undefined

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

convergerate :: Eq a => (a -> a) -> a -> [a]
convergerate f i = (takeWhile (/= (converge f i)) $ iterate f i) ++ [converge f i]

-- Converge and preserve iterations.
solve' = convergerate logTrivialize

solve''

solve = solve''
