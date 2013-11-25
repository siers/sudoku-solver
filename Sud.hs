import Data.List.Split
import Data.Tuple
import Data.List
import Control.Applicative
import System.Environment

type Line   = [Int]
type Board  = [Line]

board :: String -> Board
board = splitEvery 9 . map read . words

horizontal :: Board -> Int -> Line
horizontal = (!!)

vertical :: Board -> Int -> Line
vertical = horizontal . transpose

sqLine segment = map (+segment*3) [0..2]
sqAt   b       = (!!) . (b !!)

square :: Board -> (Int, Int) -> Line
square b (x, y) = map (sqAt b) (sqLine y) <*> (sqLine x)

squareFind i = swap $ divMod i 3

choice' fun
    | fun == "h" = horizontal
    | fun == "v" = vertical
    | fun == "s" = (. squareFind) . square
    | otherwise  = error "argument missing: h | v |s"

choice (action:arg:_) = (`fun` (read arg))
    where fun = (choice' action)

main = getArgs >>= \args -> fmap ((choice args) . board) getContents >>= putStrLn . show
