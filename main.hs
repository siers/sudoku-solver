import System.Environment
import Sudoku
import Solver

(>$>) = flip fmap

b = boardS "0 0 0 9 4 2 5 1 0 5 9 0 0 0 0 0 6 2 1 0 7 0 0 8 4 0 9 0 0 0 0 8 0 0 2 0 0 4 0 1 0 5 0 8 0 0 1 0 0 7 0 0 0 0 8 0 1 3 0 0 2 0 4 4 7 0 0 0 0 0 9 1 0 5 3 2 1 4 0 0 0"

-- ./main < sudoku
main = getContents >$> boardS >>= mapM (putStrLn . (++"\n") . showB) . convergerate trivializer
