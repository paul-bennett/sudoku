module Sudoku 
  ( Sudoku
  , mkSudoku
  , isCompleted
  , isSolved
  , solve
  , showWorkings
  ) where
  
import Data.Array
import Data.List
import Data.List.Split (chunksOf)
import Data.Char
import Control.Monad

  
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
      showElem _   = head placeholders  -- unresolved cells
      
      rank = sudokuRank s
      
      gridify s = chunksOf rank $
                  chunksOf rank $
                  chunksOf rank
                  s
      pretty g = intercalate "\n\n"
                 $ map (intercalate "\n")
                 $ map (map (intercalate "   "))
                 g


showWorkings s = concat [ gridRowStart
                        , intercalate gridRowSep (map (showSubgridRow s) [1..rank])
                        , gridRowEnd
                        ]
  where
    rank = sudokuRank s
    
    showSubgridRow s gr = intercalate cellRowSep (map (showCellRow s gr) [1..rank])
    showCellRow s gr cr = concat (map (showCandidateRow s gr cr) [1..rank])
    showCandidateRow s gr cr sr = dataRow $ chunksOf rank shownCandidates
      where
        symbolsForRow = take rank $ drop ((sr-1)*rank) (sudokuSymbols s)
        indexes = range ((gr,cr,1,1),(gr,cr,rank,rank))
        candidates = (map (sudokuGrid s!) indexes)
        shownCandidates = map foo candidates
        foo c = map (\sym -> if sym `elem` c then sym else ' ') symbolsForRow

    subgridSepData = chunksOf rank $ take (rank*rank)
                          $ repeat $ take rank $ repeat '-'
                          
    gridRowStart  = prettyRow ",-" "-+-" "-.    ,-" "-.\n" subgridSepData
    gridRowEnd    = prettyRow "`-" "-+-" "-'    `-" "-'\n" subgridSepData
    cellRowSep    = prettyRow "+-" "-+-" "-+    +-" "-+\n" subgridSepData
    dataRow d     = prettyRow "| " " | " " |    | " " |\n" d
    
    gridRowSep = concat [ gridRowEnd
                        , "\n\n"
                        , gridRowStart
                        ]

    prettyRow rowStart cellSep gridSep rowEnd d =
      concat [ rowStart
             , intercalate gridSep $ map (intercalate cellSep) d
             , rowEnd
             ]

mkSudoku :: String -> Maybe Sudoku
mkSudoku s = 
  if ( round(fromIntegral(rank) ** 4) == gridSize)
     && length specifiedSymbols <= subgridSize
  then Just Sudoku 
         { sudokuRank     = rank
         , sudokuSymbols  = sort ourSymbols
         , sudokuIndexes  = indexes
         , sudokuGrid     = array dimensions $ zip indexes cells 
  }
  else Nothing 
  where
    strippedGrid = filter (not . isSpace) s
    gridSize = length strippedGrid
    subgridSize = round $ sqrt $ fromIntegral gridSize
    rank = round $ sqrt $ fromIntegral subgridSize
    specifiedSymbols = nub $ filter (`notElem` placeholders) strippedGrid
    ourSymbols = take subgridSize $ nub $ specifiedSymbols ++ allSymbols
    dimensions = ((1,1,1,1),(rank,rank,rank,rank))
    indexes = range dimensions
    cells = map (\c -> if c `elem` placeholders
                       then ourSymbols 
                       else [c])
                strippedGrid

-- Used as placeholders on input; first is used as placeholder on output
placeholders = ['_', '.']

-- All potential symbols if not specified; numbers first, then letters
-- followed by all other punctuation in 7-bit ASCII.  Remember to remove
-- the placeholder chars
allSymbols = filter (`notElem` placeholders)
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

isSolved s = all (\g -> sortedSymbols == (sort . concat) g) gs
  where
    grid = sudokuGrid s
    sortedSymbols = sort $ sudokuSymbols s
    gs = map (\gixes -> map (grid!) gixes) (groupIndices s)
    

-- We need all the rows, all the columns and all the subgrids
-- The complete top row is (1,1,_,_)
-- and the complete last column is (_,_,3,3)
-- while the middle subgrid is (2,_,2,_)
groupIndices :: Sudoku -> [[SudokuIx]]
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

type SudokuSolver = Sudoku -> Sudoku
    
-- The big bad solver.
-- Tries all solvers until no further changes are made.
solve s | s == s'   = s
        | otherwise = solve s'
  where s' = foldr (repeatedlySolve) s
              [ solveGroups
              , reduceAllIntersections
              , solveXwing
              ]
--        repeatedlySolve solver s = solver s
        repeatedlySolve solver s | s == s' = s
                                 | otherwise = repeatedlySolve solver s'
          where s' = solver s
-- TODO: make the above more efficient.  For example, test for a solved puzzle before trying
-- a solver.

solveGroups :: SudokuSolver
solveGroups s = solveGroups' s (groupIndices s)

-- For each group in s, try to solve it then patch it back into s.
-- Stop after a single round of this (one try per group).
solveGroups' = foldr solveOneGroup
  where
    solveOneGroup gix s = s { sudokuGrid = newGrid }
      where
        group = map (oldGrid!) gix
        oldGrid = sudokuGrid s
        newGrid = oldGrid // (zip gix $ reduceGroup group)


-- Performs a single round of reductions
-- This is a generalisation of the previous two algorithms
--
-- reduceGroup considers subsets of the candidates for all cells within
-- a group, and looks for a situation where the set of cells that contain
-- those candidates is the same size as the number of candidates in the
-- subset.  In that case, any other candidates can be eliminated from that
-- set of cells.

-- In the case where the candidate subset's size is one, this has the same
-- effect as the previous setUniqueCandidates algorithm.
--
--    setUniqueCandidates ["12345", "1234", "124", "124", "24"]
--      == ["5", "3", "124", "124", "24"]

-- I am not yet 100% convinced that this algorithm is a superset of the
-- previous eliminateCandidate strategy.

-- eliminateCandidate:
--
-- If a group has n cells that share the same candidate list,
-- and that candidate list has n members, then those candidates
-- cannot be viable candidates in other cells in the group
--
--    eliminateCandidates ["56", "56", "5678", "8"]
--      == ["56", "56", "7", "8"]

reduceGroup :: [[Char]] -> [[Char]]
reduceGroup g = map snd $ foldr checkSubseq start seqs
  where
    start = zip [0..] g
    -- Split g into two groups: the cells that contain one or more
    -- members of seq and ones that don't.  If the number of cells
    -- in the former set equals the length of the sequence, then
    -- candidates not in the sequence can be filtered out of the
    -- first group

    seqs = filter (/= []) $ subsequences syms
    syms = nub $ concat g

    checkSubseq :: [Char] -> [(Int, [Char])] -> [(Int, [Char])]
    checkSubseq seq g = sortBy (\(a,_) (b,_) -> compare a b) (filtered ++ (snd parts))
      where
        -- fst parts == cells that have one or more of this seq
        -- snd parts == cells with no candidates in seq
        parts = partition (\cell -> length ((snd cell) `intersect` seq) > 0) g
        filtered = if length (fst parts) == length seq
                   then map (\(ix,cs) -> (ix, filter (`elem` seq) cs)) (fst parts)
                   else fst parts

-- Given two intersecting groups, if a particular candidate is
-- present in the intersection but not in the difference of one
-- group then it shouldn't be in the difference on the other side 
-- either.
--
-- Consider this example:
--                     ,-------------.
--                     |             |
--       .   .   .     |  .   .   .  |     1   .   .
--                     |             |
--       .   1   1     |  .   .  >1< |     .   .   .
--  ,------------------+-------------+------------------.
--  |    .   .   .     |  1   1   .  |     .   .   .    |
--  `------------------+-------------+------------------'
--                     `-------------'
--
-- In the central square, "1" is only a candidate where the square
-- intersects with the bottom row.  Therefore it can't be a candidate
-- elsewhere in the central square so the candiate marked >1< can be
-- removed.
--
-- Similarly the >2<s marked in this example can be removed:
--    ,-------------.
--  ,-+-------------+-----------------------------------.
--  | |  2   .   2  |     .  >2< >2<       .   .   .    |
--  `-+-------------+-----------------------------------'
--    |  .   .   .  |     2   .   .        .   .   2
--    |             |               
--    |  .   .   .  |     2   2   2        .   .   2
--    |             |             
--    `-------------'

-- This algorithm is useful only for pairs of groups where there are
-- three cells in the intersection, but is harmless when there is only
-- a single intersecting cell.  (In that case, intersecting cell becomes
-- committed and the candidates should be removed from the difference
-- anyway.)
--
-- Ultimately this might allow for the removal of the uniqueCandidates
-- strategy.
reduceAllIntersections :: SudokuSolver
reduceAllIntersections s = foldr reduceIntersection s
    [(g1,g2) | g1 <- groupList, g2 <- groupList, g1 /= g2]
  where
    groupList = groupIndices s

-- Applies a reduction of a pair of groups.
--   reduceIntersection (g1, g2) s = s'
--
-- Given a Sudoku s, and two groups of indices g1 and g2, if there are
-- candidates present in the intersection of g1 and g2 that aren't in
-- the rest of g1, then remove them from the rest of g2 in the result
-- Sudoku.
--
-- This also copes with non-intersecting groups.
--
reduceIntersection :: ([SudokuIx], [SudokuIx]) -> Sudoku -> Sudoku
reduceIntersection (g1, g2) s = foldl checkCandidate s candidates
  where
    ixIntersect = g1 `intersect` g2    
    ixJustG1 = g1 \\ g2
    ixJustG2 = g2 \\ g1

    -- all the candidates in the intersection  
    candidates = nub $ concat $ map (sudokuGrid s!) ixIntersect
    
    -- Apply the rule for the present candidate
    checkCandidate s c | inJustG1   = s
                       | otherwise  = s { sudokuGrid = newGrid }
      where
        inJustG1 = any (elem c) $ map (oldGrid!) ixJustG1
        
        oldGrid = sudokuGrid s
        newGrid = oldGrid // map (\ix -> (ix, oldGrid!ix \\ [c])) ixJustG2


-- General Xwing strategy
--
-- Consider the intersections of two rows and two columns.
-- If a particular candidate is present at all intersection
-- points, but otherwise absent from the two rows in question,
-- the candidate cannot be present from elsewhere in the two
-- columns.
--
-- Thus in the following example, the >3<s marked can be
-- eliminated.
--
--     ,---.                              ,---.
--     |   |                              |   |
--     | . | .   3        3   .   .       | . | 3   3
--   ,-+---+------------------------------+---+---------.
--   | | 3 | .   .        .   .   .       | 3 | .   .   |
--   `-+---+------------------------------+---+---------'
--     |>3<| .   .        3   .   .       |>3<| 3   .
--     |   |                              |   |
--     |   |                              |   |
--     |   |                              |   |
--     | . | 3   .        .   .   .       | . | .   .
--     |   |                              |   |
--     | . | .   .        .   .   .       |>3<| .   3
--     |   |                              |   |
--     | . | .   .        .   .   3       | . | .   .
--     |   |                              |   |
--     |   |                              |   |
--     |   |                              |   |
--     | . | .   3        .   .   .       | . | 3   3
--     |   |                              |   |
--     | . | .   .        .   3   .       | . | .   .
--   ,-+---+------------------------------+---+---------.
--   | | 3 | .   .        .   .   .       | 3 | .   .   |
--   `-+---+------------------------------+---+---------'
--     `---'                              `---'
--
-- Note that although the example above shows intersections
-- occurring in different subgrids, it is not necessary to test
-- for this.  For example if all intersections were in the same 
-- subgrid the puzzle would already be unsolvable because after
-- committing one intersection the candidate would need to be
-- removed from all others, by definition leaving the other row
-- without any cells with that symbol as a candidate.
--
--     ,---.   ,---.
--   ,-+---+---+---+------------------------------------.
--   | | 4 | . | 4 |      .   .   .         .   .   .   |
--   `-+---+---+---+------------------------------------'
--     | . | . | . |      .   .   .         .   .   .
--   ,-+---+---+---+------------------------------------.
--   | | 4 | . | 4 |      .   .   .         .   .   .   |
--   `-+---+---+---+------------------------------------'
--     `---'   `---'
--
-- If the intersections are in different subgrids, but in the
-- same column of subgrids the puzzle may still be solvable,
-- but the algorithm described will have no ill effect. 
--     ,---.   ,---.
--   ,-+---+---+---+------------------------------------.
--   | | 5 | . | 5 |      .   .   .         .   .   .   |
--   `-+---+---+---+------------------------------------'
--     | . | . | . |      .   .   .         .   .   .
--     |   |   |   |
--     | . | . | . |      .   .   .         .   .   .
--     |   |   |   |
--     |   |   |   |
--     |   |   |   |
--     | . | . | . |      .   .   .         .   .   .
--   ,-+---+---+---+------------------------------------.
--   | | 5 | . | 5 |      .   .   .         .   .   .   |
--   `-+---+---+---+------------------------------------'
--     | . | . | . |      .   .   .         .   .   .
--     |   |   |   |
--     |   |   |   |
--     |   |   |   |
--     | . | . |>5<|      .   .   .         .   .   .
--     |   |   |   |
--     | . | . | . |      .   .   .         .   .   .
--     |   |   |   |
--     | . | . | . |      .   .   .         .   .   .
--     `---'   `---'
-- 
-- Clearly an equivalent strategy applies by swapping the roles
-- of columns and rows.  The algorithm can be generalised further
-- by considering intersecting rows/subgrids and columns/subgrids.
-- The added complication is that in such cases there are three
-- cells in each intersecting area.
--
--     ,-----------.    ,-----------.
--   ,-+-----------+----+-----------+-------------------.
--   | | .   6   6 |    | .   .   6 |       .   .   .   |
--   `-+-----------+----+-----------+-------------------'
--     | .   .   . |    | .   .   . |       .   .   .
--   ,-+-----------+----+-----------+-------------------.
--   | | .   6   . |    | 6   .   . |       .   .   .   |
--   `-+-----------+----+-----------+-------------------'
--     `-----------'    `-----------'
--
-- The general rule is thus, given a pair of groups of the same
-- type (rows, columns or subgrids), and another pair of groups
-- of a different type, then if a particular symbol appears as
-- a candidate in the four intersection areas, but *nowhere else*
-- within the first pair of groups, then it can be eliminated
-- from the non-intersecting cells in the second pair of groups.
--
-- Phrased in this way the strategy appears very similar to the
-- general intersection solved implemented by reduceIntersection.


--solveXwing :: SudokuSolver
--solveXwing s = foldr solveXwingForSym s $ sudokuSymbols s
--  where
--    -- try to find an Xwing patter utilising symbol c in s and solve it
--    solveXwingForSym c s = s
--
--groupIndices' s = [subgrids, rows, cols]
--  where 
--    subgrids = [ range ((r,1,c,1),(r,rank,c,rank)) 
--                 | r <- ixs, c <- ixs]
--    rows     = [ range ((sg,cell,1,1),(sg,cell,rank,rank)) 
--                 | sg <- ixs, cell <- ixs]
--    cols     = [ range ((1,1,sg,cell),(rank,rank,sg,cell)) 
--                 | sg <- ixs, cell <- ixs]
--    rank = sudokuRank s
--    ixs = [1..rank]

solveXwing :: SudokuSolver
solveXwing s = foldr solveXwing' s $ groupIndices s
  where
    solveXwing' :: [SudokuIx] -> Sudoku -> Sudoku
    solveXwing' g s = foldr (\(g1,g2,g3,g4) s -> reduceXwingIntersection g1 g2 g3 g4 s) s combs
    
    combs = combsCR
--    combsFixed = [(gs!!10, gs!!17, gs!!18, gs!!24)]   -- for 2020-01-06
--    combsNaive = (,,,) <$> gs <*> gs <*> gs <*> gs
--    combsPerm = map (\[a,b,c,d] -> (a,b,c,d)) $ concat $ map permutations $ filter (\g -> 4 == length g) (subsequences gs)
    combsType rt ct = [(r1,r2,c1,c2) | r1 <- rt, r2 <- rt, r1 /= r2
                                     , c1 <- ct, c2 <- ct, c1 /= c2]
    combsCR = (combsType cols rows) ++ (combsType rows cols)
                             
              
    rank = sudokuRank s             
    ixs = [1..rank]
    rows     = [ range ((sg,cell,1,1),(sg,cell,rank,rank)) 
                 | sg <- ixs, cell <- ixs]
    cols     = [ range ((1,1,sg,cell),(rank,rank,sg,cell)) 
                 | sg <- ixs, cell <- ixs]

    gs = groupIndices s

-- Apply the Xwing reduction for the specified four groups.
-- The groups might not intersect, so 
reduceXwingIntersection
  :: [SudokuIx] -> [SudokuIx] -> [SudokuIx] -> [SudokuIx]
  -> Sudoku -> Sudoku
reduceXwingIntersection g11 g12 g21 g22 s 
  | any null intersectIxes  = s
  | otherwise               = foldr reduceXwing' s candidateSyms
  where
    oldGrid = sudokuGrid s
    intersectIxes = intersect <$> [g11, g12] <*> [g21, g22]
    intersectVals = liftM (nub . concat . (map (oldGrid!))) intersectIxes
    candidateSyms = foldr intersect (sudokuSymbols s) intersectVals  -- syms at all intersections
    
    -- Is Xwing applicable for the candidate symbol?  If so, go for it.
    -- We already know that c is present at all intersections, so the
    -- qualifying test is to ensure it's not present elsewhere in g11 and g12
    
    reduceXwing' :: Char -> Sudoku -> Sudoku
    reduceXwing' c s | c `elem` otherRowVals = s
                     | otherwise             = s { sudokuGrid = newGrid }
      where
        -- 'row' and 'col' here are just convenient names; may be rows, cols
        -- or subgrids depending on the groups passed into reduceXwingIntersection
        otherRowIxes = (g11 ++ g12) \\ concat intersectIxes
        otherRowVals = concat $ map (oldGrid!) otherRowIxes
        
        otherColIxes = (g21 ++ g22) \\ concat intersectIxes
        colUpdates = map (\ix -> (ix, filter (/= c) (oldGrid!ix))) otherColIxes

        newGrid = oldGrid // colUpdates

-- TODO: improve efficiency of Xwing reduction
-- TODO: consider similarities between Xwing intersection and general intersection code
-- TODO: improve selection of groups for Xwing reduction (specifically subgrids/rows,cols)    

--reduceIntersection :: ([SudokuIx], [SudokuIx]) -> Sudoku -> Sudoku
--reduceIntersection (g1, g2) s = foldl checkCandidate s candidates
--  where
--    ixIntersect = g1 `intersect` g2    
--    ixJustG1 = g1 \\ g2
--    ixJustG2 = g2 \\ g1
--
--    -- all the candidates in the intersection  
--    candidates = nub $ concat $ map (sudokuGrid s!) ixIntersect
--    
--    -- Apply the rule for the present candidate
--    checkCandidate s c | inJustG1   = s
--                       | otherwise  = s { sudokuGrid = newGrid }
--      where
--        inJustG1 = any (elem c) $ map (oldGrid!) ixJustG1
--        
--        oldGrid = sudokuGrid s
--        newGrid = oldGrid // map (\ix -> (ix, oldGrid!ix \\ [c])) ixJustG2

