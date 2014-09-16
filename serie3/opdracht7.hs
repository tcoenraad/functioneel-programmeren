import FPPrac
import RoseTree

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

pp1a :: Tree1a -> RoseTree
pp1a (Node1a i t1 t2) = RoseNode (show i) [(pp1a t1), (pp1a t2)]
pp1a (Leaf1a i) = RoseNode (show i) []

--showTree(pp1a(Node1a 1 (Node1a 5 (Leaf1a 8) (Leaf1a 3)) (Leaf1a 3)))

vervang :: String -> Number -> Tree1a -> Tree1a
vervang p n (Node1a i t1 t2) | p == "" = Node1a n t1 t2
                             | (head p) == 'l' = Node1a i (vervang (tail p) n t1) t2
                             | otherwise = Node1a i t1 (vervang (tail p) n t2)                            
vervang p n (Leaf1a i) = Leaf1a i

--showTree(pp1a(vervang "l" 2 (Node1a 1 (Node1a 5 (Leaf1a 8) (Leaf1a 3)) (Leaf1a 3))))

subboom :: String -> Tree1a -> Tree1a
subboom p (Node1a i t1 t2) | p == "" = Node1a i t1 t2
                           | (head p) == 'l' && (length p) > 0 = subboom (tail p) t1
                           | (head p) == 'r' && (length p) > 0 = subboom (tail p) t2
subboom p (Leaf1a i) | p == "" = Leaf1a i
                     | otherwise = error "lengte pad groter dan boom"

--showTree(pp1a(subboom "lrl" (Node1a 1 (Node1a 5 (Leaf1a 8) (Leaf1a 3)) (Leaf1a 3))))

linkerboom :: String -> Tree1a -> Tree1a
linkerboom p (Node1a i t1 t2) | elem 'r' p == False = error "gegeven blad heeft geen linkerbuur"
                              | otherwise = vervangblad (reverse (linkerboom' (reverse p))) (-1) (Node1a i t1 t2)

linkerboom' :: String -> String
linkerboom' [] = []
linkerboom' p | head p == 'l' = "r" ++ linkerboom' (tail p) 
              | otherwise = "l" ++ tail p

vervangblad :: String -> Number -> Tree1a -> Tree1a
vervangblad p n (Node1a i t1 t2) | p == "" = error "gegeven blad bestaat niet"
                                 | (head p) == 'l' = Node1a i (vervangblad (tail p) n t1) t2
                                 | otherwise = Node1a i t1 (vervangblad (tail p) n t2)                            
vervangblad p n (Leaf1a i) | p == "" = Leaf1a n
                           | otherwise = error "gegeven blad bestaat niet"
                    
--showTree(pp1a(linkerboom "lr" (Node1a 1 (Node1a 2 (Leaf1a 4) (Leaf1a 5)) (Node1a 3 (Leaf1a 6) (Leaf1a 7)))))
--showTree(pp1a(Node1a 1 (Node1a 2 (Leaf1a 4) (Leaf1a 5)) (Node1a 3 (Leaf1a 6) (Leaf1a 7))))

rechterboom :: String -> Tree1a -> Tree1a
rechterboom p (Node1a i t1 t2) | elem 'l' p == False = error "gegeven blad heeft geen linkerbuur"
                              | otherwise = vervangblad (reverse (rechterboom' (reverse p))) (-1) (Node1a i t1 t2)

rechterboom' :: String -> String
rechterboom' [] = []
rechterboom' p | head p == 'r' = "l" ++ rechterboom' (tail p) 
              | otherwise = "r" ++ tail p

--showTree(pp1a(rechterboom "lr" (Node1a 1 (Node1a 2 (Leaf1a 4) (Leaf1a 5)) (Node1a 3 (Leaf1a 6) (Leaf1a 7)))))