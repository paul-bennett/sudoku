module Main where

-- Sudoku solver
--
-- Never guesses.  Uses just two deductive rules.  I've not tried
-- to prove whether or not that's enough to solve all puzzles with
-- unique solutions. 
--
-- Run like this:
--   $ cat <<EOT > sudoku
--   53_ _7_ ___
--   6__ 195 ___
--   _98 ___ _6_
--              
--   8__ _6_ __3
--   4__ 8_3 __1
--   7__ _2_ __6
--             
--   _6_ ___ 28_
--   ___ 419 __5
--   ___ _8_ _79 
--   EOT
--
-- The code can work with puzzles of any rank (2x2, 3x3, 4x4, ...).
-- Input data is a sequence of cells where "_" represents an unknown
-- cell value.  Whitespace is ignored in input, but the number of
-- cells must be exactly rank^4.  Character set can be anything
-- (but "_" is always "unknown").
--
-- Input lines beginning with a # character are ignored as comments.

import Sudoku  
import Data.Maybe
import Data.List

main :: IO ()
main = do
  input <- getContents
  
  let cleanInput = unlines $ filter (not . ("#" `isPrefixOf`)) (lines input)
  let puzzle = fromJust $ mkSudoku cleanInput
  let solution = solve puzzle
  
  putStrLn $ (if isSolved solution then "" else "Partial ") ++ "Solution:"
  putStrLn $ show $ solve puzzle
  
  putStrLn $ (if isSolved solution then "" else "\nWorking:\n" ++ showWorkings solution)
  