module Trie ( Trie
            , empty
            , singleton
            , addToTrie
            , Trie.lookup) where

import qualified Data.Map as Map
import Data.Maybe

data Trie a = Trie { value :: Maybe a
                   , children :: Map.Map Char (Trie a) } deriving (Show)

empty :: Trie a
empty = Trie {value = Nothing, children = Map.empty}

singleton :: a -> Trie a
singleton x = Trie {value = Just x, children = Map.empty}

addToTrie :: String -> Bool -> Trie Bool -> Trie Bool
addToTrie [x] val trie = Trie (value trie) (Map.insert x (singleton val) (children trie))
addToTrie all@(x:xs) val trie
    | Map.member x (children trie) = Trie currentVal (Map.insert x (addToTrie xs val subTreeForX) childTries)
    | otherwise = 
        let addChild = Map.insert x (singleton False) childTries
        in addToTrie all val (Trie currentVal addChild)
    where childTries = children trie
          currentVal = value trie
          subTreeForX = fromJust $ Map.lookup x childTries
          
lookup :: String -> Maybe (Trie Bool) -> Maybe Bool
lookup _ Nothing          = Nothing
lookup [] (Just trie)     = value trie
lookup (x:xs) (Just trie) =
    Trie.lookup xs (Map.lookup x (children trie))


