--EC1.hs
--Nicholas Latham
--implemantation of a heap

data Heap a = Node a (Heap a) (Heap a)
            |Nil
            deriving(Show, Eq)

--insert
--inserts a node into a heap
insert :: Ord a => a -> Heap a -> Heap a
insert a (Node n Nil r) = trickleUp 'l' (Node n (Node a Nil Nil) r)
insert a (Node n l Nil) = trickleUp 'r' (Node n l (Node a Nil Nil))
insert a (Node n l r)
  |not (isFull l) = trickleUp 'l' (Node n (insert a l) r)
  |height l > height r = trickleUp 'r' (Node n l (insert a r))
  |not (isFull r) = trickleUp 'r' (Node n l (insert a r))
  |otherwise = trickleUp 'l' (Node n (insert a l) r)

--trickleUp
--takes in a character and a node and compares values of the node
--if the character is l then it compares parent and left child
--if the character is r then it compares parent and right child
--the default is r
trickleUp :: Ord a => Char -> Heap a -> Heap a
trickleUp 'l' (Node a (Node b bl br) r)
  |a > b = Node b (Node a bl br) r
  |otherwise = Node a (Node b bl br) r
trickleUp _ (Node a l (Node b bl br))
  |a > b = Node b l (Node a bl br)
  |otherwise = (Node a l (Node b bl br))

--height
--gets the height of the tree
height :: Heap a -> Int
height Nil = 0
height (Node _ l r) = 1 + max (height l) (height r)

--isFull
--checks to see of the tree is full
isFull :: Heap a -> Bool
isFull Nil = True
isFull (Node _ Nil Nil) = True
isFull (Node _ l Nil) = False
isFull (Node _ Nil r) = False
isFull (Node _ l r) = isFull l && isFull r && height l == height r

--getMin
--returns the top value of a heap
getMin :: Heap a -> a
getMin Nil = error "Empty Heap"
getMin (Node a _ _) = a

--lastChild
--gets the last child of a list
lastChild :: Heap a -> a
lastChild (Node a Nil _) = a
lastChild (Node a l r)
  |not (isFull l) = lastChild l
  |(height l) > (height r) && isFull r = lastChild l
  |otherwise = lastChild r

--deleteLast
--deletes the last child of the list
deleteLast :: Heap a -> Heap a
deleteLast (Node a Nil _) = Nil
deleteLast (Node a l r)
  |not (isFull l) = Node a (deleteLast l) r
  |(height l) > (height r) && isFull r = Node a (deleteLast l) r
  |otherwise = Node a l (deleteLast r)

--deleteMin
--deletes the top of the Heap
deleteMin :: Ord a => Heap a -> Heap a
deleteMin Nil = Nil
deleteMin (Node a l Nil) = l
deleteMin (Node a l r) =
  trickleDown (deleteLast (Node (lastChild (Node a l r)) l r))

--trickleDown
--does trickle down on the heap
trickleDown :: Ord a => Heap a -> Heap a
trickleDown (Node a Nil Nil) = Node a Nil Nil
trickleDown (Node a (Node b bl br) Nil)
  |b < a = Node b (Node a bl br) Nil
  |otherwise = Node a (Node b bl br) Nil
trickleDown (Node a (Node b bl br) (Node c cl cr))
  | a <= b && a <= c = Node a (Node b bl br) (Node c cl cr)
  | b <= c && b < a = Node b (trickleDown (Node a bl br)) (Node c cl cr)
  | otherwise = Node c (Node b bl br) (trickleDown (Node a cl cr))
