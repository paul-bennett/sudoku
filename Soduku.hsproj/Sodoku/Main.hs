module Main where
  
import Data.Array
import Data.List
import Data.List.Split (chunksOf)
import Data.Char
import Data.Maybe

main :: IO ()
main = do
  input <- getContents
  -- TODO: handle Nothing case
  let puzzle = fromJust $ mkSudoku input
  putStrLn $ show puzzle
  putStrLn $ show $ solve puzzle
  

-- Cells are lexographically, top-to-bottom, left-to-right:
--
--                ,-------------- Row of subgrid
--                |
--                |   ,---------- Row within subgrid
--                |   |
--                |   |   ,------ Col of subgrid
--                |   |   |
--                |   |   |   ,-- Col within subgrid
--                |   |   |   |
--                v   v   v   v
type SudokuIx = (Int,Int,Int,Int)


data Sudoku = Sudoku
  { sudokuRank :: Int
  , sudokuSymbols :: [Char]
  , sudokuIndexes :: [SudokuIx]
  , sudokuGrid :: Array SudokuIx [Char]
  }
  deriving(Eq)
  
instance Show Sudoku where
  show s = pretty $ gridify $ map showElem $ elems $ sudokuGrid s
    where 
      showElem [e] = e    -- cell with only one possible answer
      showElem _   = '_'  -- show a placeholder for unresolved cells
      
      rank = sudokuRank s
      
      -- TODO: is there a higher-level function that simplifies
      -- the repetition of gridify and pretty?
      gridify s = chunksOf rank $
                  chunksOf rank $
                  chunksOf rank
                  s
      pretty g = intercalate "\n\n"
                 $ map (intercalate "\n")
                 $ map (map (intercalate "   "))
                 g


mkSudoku :: String -> Maybe Sudoku
mkSudoku s = 
  if ( round(fromIntegral(rank) ** 4) == gridSize)
     && length specifiedSymbols <= subgridSize
  then Just Sudoku 
         { sudokuRank     = rank
         , sudokuSymbols  = ourSymbols
         , sudokuIndexes  = indexes
         , sudokuGrid     = array dimensions $ zip indexes cells 
  }
  else Nothing 
  where
    strippedGrid = filter (not . isSpace) s
    gridSize = length strippedGrid
    subgridSize = round $ sqrt $ fromIntegral gridSize
    rank = round $ sqrt $ fromIntegral subgridSize
    specifiedSymbols = nub $ filter (/= placeholder) strippedGrid
    ourSymbols = take subgridSize $ nub $ specifiedSymbols ++ allSymbols
    dimensions = ((1,1,1,1),(rank,rank,rank,rank))
    indexes = range dimensions
    cells = map (\c -> if c == placeholder
                       then ourSymbols 
                       else [c])
                strippedGrid

-- Used as a placeholder on input
placeholder = '_'

-- All potential symbols if not specified; numbers first, then letters
-- followed by all other punctuation in 7-bit ASCII.  Remember to remove
-- the placeholder char
allSymbols = filter (/= placeholder)
             $ nub
             $ ['1'..'9'] ++ "0" ++ 
               ['a'..'z'] ++ ['A'..'Z'] ++
               ['\x21'..'\x7e']

-- Have all the cells got exactly one candidate specified?
isCompleted :: Sudoku -> Bool
isCompleted s = all (\c -> length c == 1) $ elems $ sudokuGrid s

-- Are all the groups correct?  I.E. for each group, is the
-- number of distinct 
isSolved :: Sudoku -> Bool

isSolved s = all (\g -> (sort symbols) == sort (foldr (++) "" g)) gs
  where
    grid = sudokuGrid s
    symbols = sudokuSymbols s
    gs = map (\gixes -> map (grid!) gixes) (groupIndices s)
    

-- We need all the rows, all the columns and all the subgrids
-- The complete top row is (1,1,_,_)
-- and the complete last column is (_,_,3,3)
-- while the middle subgrid is (2,_,2,_)
groupIndices s = subgrids ++ rows ++ cols
  where 
    subgrids = [ range ((r,1,c,1),(r,rank,c,rank)) 
                 | r <- ixs, c <- ixs]
    rows     = [ range ((sg,cell,1,1),(sg,cell,rank,rank)) 
                 | sg <- ixs, cell <- ixs]
    cols     = [ range ((1,1,sg,cell),(rank,rank,sg,cell)) 
                 | sg <- ixs, cell <- ixs]
    rank = sudokuRank s
    ixs = [1..rank]
    
-- The big bad solver.
-- Keeps trying solve' until no further changes are made.
solve :: Sudoku -> Sudoku
solve s | s == s'   = s
        | otherwise = solve s'
  where
    s'   = solve' ixes s
    ixes = groupIndices s
    
-- For each group in s, try to solve it then patch it back into s.
-- Stop after a single round of this (one try per group).
solve' [] s = s
solve' (gix:gixs) s = solve' gixs (s { sudokuGrid = grid' })
  where
    group = map (grid!) gix
    grid = sudokuGrid s
    group' = reduceGroup group
    grid' = grid // (zip gix group')    

-- group is a list of strings. Each string is a list of possible
-- values for that place. Goal is to eliminate duplicates.  Do this
-- by finding singleton strings, then remove that value from all
-- other strings in the group.

-- Keep reducing until no changes have been made
reduceGroup g | g == g'   = g
              | otherwise = reduceGroup g'
  where
    g' = reduceGroup' g

-- Performs a single round of reductions
-- In each round, apply two algorithms.
reduceGroup' = eliminateCandidates . setUniqueCandidates

-- If a group has n cells that share the same candidate list,
-- and that candidate list has n members, then those candidates
-- cannot be viable candidates in other cells in the group
--    eliminateCandidates ["56", "56", "5678", "8"]
--      == ["56", "56", "7", "8"]
eliminateCandidates g = reverse (elc' [] g)
  where
    elc' a [] = a
    elc' a (h:t) = elc' (h:a') t'
      where
        n = length h
        c = 1 + length (filter (h==) (a++t))
        a' = s a
        t' = s t
        s = if n /= c
            then id
            else map (\x -> if    x == h 
                            then  x
                            else  filter (`notElem` h) x
                          )
                      
-- If there's a symbol that only appears in one cell's candidate
-- list, then it must be the value of that cell.
--    setUniqueCandidates ["12345", "1234", "124", "124"] 
--      == ["5", "3", "124", "124"]
setUniqueCandidates g = suc' symbols g
  where
    symbols = nub $ foldr (++) "" g
    suc' [] g                     = g
    suc' (sym:rest) g | c==1      = suc' rest $ (map f g)
                      | otherwise = suc' rest g
      where
        -- How many members of the group included sym?
        c = length (filter (sym `elem`) g)
    
        f e = if sym `elem` e
              then [sym]
              else e