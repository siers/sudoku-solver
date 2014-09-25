import System.Environment
import Sudoku
import Solver

-- ./main < sudoku
main = putStrLn . showB . solve . boardS =<< getContents
