import Data.Char
import Data.List (nub)
import Data.Foldable
import Data.Maybe
import Data.Sequence
import qualified Data.Map as Map
import Debug.Trace

import Trie

data GridCell = GridCell { letter :: Char
                         , visited :: Bool } deriving (Show, Eq)

gridSize = 4

grid = [['N','R','Y','X'],
        ['U','T','Q','F'],
        ['A','O','S','H'],
        ['E','K','M','A']]

grid2 = [['N','R'],
         ['U','T']]

-- given a cell x,y
-- generate a list of adjacent cells
-- generate a new grid with the cell (x,y) marked visited
-- for each adjacent cell, if that cell has not been visited, and that cell is a prefix, recurse
-- fold up the results from each of the adjacent cells into one list
depthFirstSearch :: (Int, Int) -> [[GridCell]] -> Trie Bool -> String -> [String] -> [String]
depthFirstSearch (x,y) grid trie progress words =
    foldl (\acc w -> if w == [""] then acc else acc ++ w) [if isWord == Just True then z else []] (map (\(i,j) ->
        if visited (grid !! j !! i) || isNothing isWord then [""]
            else depthFirstSearch (i,j) newGrid trie z words) adjacent)
    where adjacent = getAdjacent (x,y) (gridSize, gridSize)
          newGrid = markVisited (x,y) grid
          currentLetter = letter (grid !! y !! x)
          z = progress ++ [currentLetter]
          isWord = Trie.lookup z (Just trie)

-- Creates a new 2D array of GridCells. 
-- take row y of the grid, turn it into a sequence (which allows updating elements), replace
-- the xth element with a new GridCell with the same letter but visited == true
markVisited :: (Int, Int) -> [[GridCell]] -> [[GridCell]]
markVisited (x,y) grid = 
    map (\i -> if i /= line then i else toList $ update x newGridCell (fromList i)) grid
    where line = grid !! y
          toMark = line !! x
          newGridCell = GridCell (letter toMark) True

getAdjacent :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getAdjacent (x, y) (width, height) =
    [(i,j) | i <- [x - 1..x + 1], j <- [y - 1..y + 1],
        i >= 0, j >= 0, i < width, j < height, x /= i || y /= j]

toGridCell :: [String] -> [[GridCell]]
toGridCell grid = [ [ GridCell x False | x <- line ] | line <- grid] 

buildTrie :: [String] -> Trie Bool -> Trie Bool
buildTrie words t = foldl (\t x -> addToTrie x True t) t words

main = do
    wordFile <- readFile  "words.txt"
    let words = lines wordFile
    let wordTrie = buildTrie words (Trie.singleton False)
    --print $ Trie.lookup "ADDITION" (Just wordTrie)
    -- print $ Trie.lookup "ADSDF" (Just wordTrie)
    -- print $ Trie.lookup "ASA" (Just wordTrie)

    --print wordTrie
    let gridCells = toGridCell grid
    let cells = [(i,j) | i <- [0..gridSize-1], j <- [0..gridSize-1]]
    let result = map (\(i, j) -> depthFirstSearch (i,j) gridCells wordTrie "" []) cells
    print result
