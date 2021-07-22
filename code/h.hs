data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert newNodeValue EmptyTree = singleton newNodeValue -- deconstructing "a -> Tree a" and return a singleton with newNodeValue
treeInsert newNodeValue (Node a left right) -- deconstructing "a -> Tree a"
    | newNodeValue == a = Node newNodeValue left right -- call the Node constructor with the value and two trees
    | newNodeValue < a  = Node a (treeInsert newNodeValue left) right
    | newNodeValue > a  = Node a left (treeInsert newNodeValue right)
