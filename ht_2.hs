module Main where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)

data AVLTree a
  = Empty
  | Node (AVLTree a) a (AVLTree a) Int
  deriving (Show, Eq)

height Empty            = 0
height (Node _ _ _ h)   = h

mkNode :: AVLTree a -> a -> AVLTree a -> AVLTree a
mkNode l v r = Node l v r h
  where h = 1 + max (height l) (height r)

balanceFactor :: AVLTree a -> Int
balanceFactor Empty            = 0
balanceFactor (Node l _ r _)   = height l - height r

isAVL :: (Ord a) => AVLTree a -> Bool
isAVL Empty = True
isAVL (Node l _ r _) =
  abs (height l - height r) <= 1 && isAVL l && isAVL r

rotateRight :: AVLTree a -> AVLTree a
rotateRight (Node (Node a x b _) y c _) = mkNode a x (mkNode b y c)
rotateRight t = t 

rotateLeft :: AVLTree a -> AVLTree a
rotateLeft (Node a x (Node b y c _) _) = mkNode (mkNode a x b) y c
rotateLeft t = t

rotateLeftRight :: AVLTree a -> AVLTree a
rotateLeftRight (Node l v r h) = rotateRight (mkNode (rotateLeft l) v r)
rotateLeftRight t = t

rotateRightLeft :: AVLTree a -> AVLTree a
rotateRightLeft (Node l v r h) = rotateLeft (mkNode l v (rotateRight r))
rotateRightLeft t = t

rebalance :: AVLTree a -> AVLTree a
rebalance t@(Node l v r _)
  | bf > 1 && balanceFactor l >= 0 = rotateRight t
  | bf > 1 && balanceFactor l <  0 = rotateLeftRight t 
  | bf < -1 && balanceFactor r <= 0 = rotateLeft t 
  | bf < -1 && balanceFactor r >  0 = rotateRightLeft t  
  | otherwise = mkNode l v r
  where bf = balanceFactor t
rebalance t = t

insert :: Ord a => a -> AVLTree a -> AVLTree a
insert x Empty = mkNode Empty x Empty
insert x (Node l v r h)
  | x < v     = rebalance $ mkNode (insert x l) v r
  | x > v     = rebalance $ mkNode l v (insert x r)
  | otherwise = Node l v r h

findMin :: AVLTree a -> Maybe a
findMin Empty                = Nothing
findMin (Node Empty v _ _)   = Just v
findMin (Node l _ _ _)       = findMin l

removeMin :: Ord a => AVLTree a -> AVLTree a
removeMin Empty = Empty
removeMin (Node Empty _ r _) = r
removeMin (Node l v r _)     = rebalance $ mkNode (removeMin l) v r

delete :: Ord a => a -> AVLTree a -> AVLTree a
delete _ Empty = Empty
delete x (Node l v r _)
  | x < v = rebalance $ mkNode (delete x l) v r
  | x > v = rebalance $ mkNode l v (delete x r)
  | otherwise =
      case (l, r) of
        (Empty, Empty) -> Empty
        (Empty, _)     -> r
        (_, Empty)     -> l
        (_, _)         ->
          case findMin r of
            Just succVal ->
              let r' = removeMin r
              in rebalance $ mkNode l succVal r'
            Nothing -> error "right subtree expected non-empty"

toList :: AVLTree a -> [a]
toList Empty = []
toList (Node l v r _) = toList l ++ [v] ++ toList r

fromList :: Ord a => [a] -> AVLTree a
fromList = foldr insert Empty

pretty :: Show a => AVLTree a -> String
pretty = unlines . go 0
  where
    pad n = replicate (2*n) ' '
    go _ Empty = []
    go d (Node l v r _) =
      go (d+1) r ++ [pad d ++ show v] ++ go (d+1) l

main :: IO ()
main = do
  putStrLn "AVL tree demo\n"

  let elems = [10,5,2,7,6,8,15,12,18,17,16,20]
      t0 = fromList elems

  putStrLn "Initial AVL tree from list:"
  putStrLn $ pretty t0
  putStrLn $ "Inorder (sorted): " ++ show (toList t0)
  putStrLn $ "Is AVL? " ++ show (isAVL t0)
  putStrLn $ "Root height: " ++ show (height t0)
  putStrLn ""

  let t1 = foldr insert t0 [11,13,14] 
  putStrLn "After inserts (11,13,14):"
  putStrLn $ pretty t1
  putStrLn $ "Inorder: " ++ show (toList t1)
  putStrLn $ "Is AVL? " ++ show (isAVL t1)
  putStrLn $ "Root height: " ++ show (height t1)
  putStrLn ""

  let t2 = delete 15 t1
      t3 = delete 10 t2
  putStrLn "After deletes (15), then (10):"
  putStrLn $ pretty t3
  putStrLn $ "Inorder: " ++ show (toList t3)
  putStrLn $ "Is AVL? " ++ show (isAVL t3)
  putStrLn $ "Root height: " ++ show (height t3)
  putStrLn ""

  putStrLn "Edge cases:"
  putStrLn "- delete non-existing element (999) -> should keep tree unchanged"
  let t4 = delete 999 t3
  putStrLn $ "Is AVL after delete 999? " ++ show (isAVL t4)
  putStrLn $ "Same inorder: " ++ show (toList t4)
  putStrLn ""
  putStrLn "Done."