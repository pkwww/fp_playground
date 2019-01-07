data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)

preorder :: Tree a -> [a]
preorder Nil = []
preorder (Node val left right) = val : (leftList ++ rightList)
  where leftList = preorder left
        rightList = preorder right

{-preorder_cont :: Tree a -> ([a] -> r) -> r
preorder_cont Nil k = k []
predrder_cont (Node val left right) k = -}
sum_cont :: (Integer, Integer) -> (Integer -> r) -> r
sum_cont (a, b) k = k (a + b)

fact_tail :: Integer -> Integer -> Integer
fact_tail 0 v = v
fact_tail n v = fact_tail (n-1) (v*v-1)

compose :: (a -> b) -> (b -> c) -> a -> c
compose f g x = g (f x)

prod :: Integer -> Integer -> Integer
prod a b = a * b
fact_cps :: Integer -> (Integer -> r) -> r
fact_cps 0 k = k 1
fact_cps n k =  fact_cps (n-1) (k . (n *))

fact :: Integer -> Integer
fact n = foldr prod 1 [1..n]

data List a = Nul | Cons a (List a) deriving (Show)
foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' f e Nul = e
foldr' f e (Cons x xs) = foldr' f (f x e) xs

tree :: Tree Int
tree = Node 1 
  (Node 3 
    (Node 4 Nil Nil)
    (Node 5 Nil Nil)
  )
  (Node 2
    (Node 6 Nil Nil)
    Nil
  )

list = preorder tree

main :: IO ()
main = putStr (show list)