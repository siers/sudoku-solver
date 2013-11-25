import System.Environment
import Sudoku
import Solver

choice' :: String -> Board -> Int -> Line
choice' fun
    | fun == "h" = horizontal
    | fun == "v" = vertical
    | fun == "s" = (. squareFind) . square
    | otherwise  = error "argument missing: h | v |s"

choice (action:arg:_) = (`fun` (read arg))
    where fun = (choice' action)

operate :: [String] -> String -> Line
operate = (. board) . choice

main = ((`fmap` getContents) . operate =<< getArgs) >>= putStrLn . show . solve
