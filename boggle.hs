import qualified Data.Map as Map

import Trie

grid = [['N','R','Y','X'],
        ['U','T','Q','F'],
        ['A','O','S','H'],
        ['E','K','M','A']]

main = do
    let t = Trie.singleton False
    print t
    let u = addToTrie "a" True t
    print u 
    let v = addToTrie "b" False u
    print v
    let x = addToTrie "an" True v
    print x
    let y = addToTrie "and" True x
    print y
    let z = addToTrie "add" True y
    print z
    let q = addToTrie "addition" True z
    print q
    print $ Trie.lookup "add" (Just q)
    print $ Trie.lookup "ad" (Just q)
    print $ Trie.lookup "ann" (Just q)
    print $ Trie.lookup "addition" (Just q)
    print $ Trie.lookup "an" (Just q)
