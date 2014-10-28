data Graph = Graph{nodes :: [Node], edges :: [Edge]} deriving Show
type Node = Int
type Edge = (Int, Int)

cut :: Graph -> [Node] -> (Graph, Graph)
cut Graph{nodes=ns, edges=es} ns1 = (g1, g2) where
  es1 = filter (\(a,b) -> a `elem` ns1 && b `elem` ns1) es
  g1 = Graph{nodes=ns1, edges=es1}
  ns2 = filter (not . (flip elem) ns1) ns
  es2 = filter (\(a,b) -> a `elem` ns2 && b `elem` ns2) es
  g2 = Graph{nodes=ns2, edges=es2}
