import Data.Char
import Data.Foldable
import Data.List (intersperse, nub, sort)
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (update, fromList)
import System.Random

import Trie

data GridCell = GridCell { letter :: Char
                         , visited :: Bool } deriving (Show, Eq)

gridSize = 5
minWordSize = 4

generateGrid :: StdGen -> Int -> [[GridCell]]
generateGrid randGen size = [ [ GridCell char False | char <- take size (drop ((i - 1) * size) randChars) ] | i <- [1..size] ]
    where randChars = take (size * size) $ randomRs ('A','Z') randGen :: String

printGrid :: [[GridCell]] -> [String]
printGrid grid = [ [ letter gridCell | gridCell <- row ] | row <- grid]

solve :: [[GridCell]] -> Trie Bool -> [String]
solve grid wordTrie = (sort . nub) $ filter (/= "") 
    (foldl (++) [""] (map (\(i,j) -> depthFirstSearch (i,j) grid wordTrie "" []) cellCoords))
    where cellCoords = [(i,j) | i <- [0..gridSize-1], j <- [0..gridSize-1]]

-- given a cell x,y,
-- generate a list of adjacent cells
-- generate a new grid with the cell (x,y) marked visited
-- for each adjacent cell, if that cell has not been visited, and that cell is a prefix, recurse
-- fold up the results from each of the adjacent cells into one list
depthFirstSearch :: (Int, Int) -> [[GridCell]] -> Trie Bool -> String -> [String] -> [String]
depthFirstSearch (x,y) grid trie progress words =
    foldl (\acc w -> if w == [""] then acc else acc ++ w)
        [if isWord == Just True && length z >= minWordSize then z else []] (map (\(i,j) ->
            if visited (grid !! j !! i) || isPrefix then [""]
                else depthFirstSearch (i,j) newGrid trie z words) adjacent)
    where adjacent = getAdjacent (x,y) (gridSize, gridSize)
          newGrid = markVisited (x,y) grid
          currentLetter = letter (grid !! y !! x)
          z = progress ++ [currentLetter]
          isWord = Trie.lookup z (Just trie)
          isPrefix = isNothing isWord

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
    randGen <- getStdGen
    let generatedGrid = generateGrid randGen 5
    forM_ (printGrid generatedGrid) (putStrLn . intersperse ' ')
    wordFile <- readFile "words.txt"
    let words = lines wordFile
    let wordTrie = buildTrie words (Trie.singleton False)
    let result = solve generatedGrid wordTrie
    print result
