import FPPrac
import FPPrac.Graphs
import Data.Maybe
import Data.Char (isDigit)
import Data.List ((\\), delete)
    

-- | Store datatype
--   Hiermee worden alle variabelen van het
--   programmer gedefinieerd
data Store = Store 
             { pressedN :: Bool
             , pressedR :: Bool
             , pressedE :: Bool
             , pressedD :: Bool
             , pressedW :: Bool
             , pressedF :: Bool
             , pressedQ :: Bool
             , pressedZ :: Bool
             , pressedX :: Bool
             , node1Select :: Maybe Node
             , node2Select :: Maybe Node
             , graph :: Graph
             }                

             
-- | Begingraph
--   Dit is de begintoestand van de graaf             
beginGraph = Graph [('a', (50,50), Orange), ('b', (100, 100), Black), ('c', (150, 150), Blue), ('d', (100, 200), Blue)] [('a', 'b', Black, 5), ('b', 'b', Black, 5), ('c', 'd', Black, 5)] Undirected Weighted

-- | BeginStore
--   Dit is de begintoestand van de store
beginStore = Store  { pressedN = False
                    , pressedR = False
                    , pressedE = False
                    , pressedD = False
                    , pressedW = False
                    , pressedF = False
                    , pressedQ = False
                    , pressedZ = False
                    , pressedX = False
                    , node1Select = Nothing
                    , node2Select = Nothing
                    , graph   = beginGraph
                    }
 
-- | Instructions
--   Dit is een lijst van alle mogelijke 
--   instructies 
instructions = Instructions [ "Instructions",
                              "Press 'n' and click on the screen to create a new node",
                              "Press 'r', click on a node and press a letter to rename the node",
                              "Press 'e', click on two nodes to create an edge",
                              "Press 'd', click on a node to delete the node",
                              "Press 'w', click on two nodes and press a number to weight the edge in between",
                              "Press 'f', click on two nodes to delete an edge",
                              "Press 'q', click on a node to color it red",
                              "Press 'z', click on a node to color all adjacent nodes blue",
                              "Press 'x' and click on the sceen to reset everything"
                            ]                             

-- | Resetcommands
--   Deze functie reset alle mogelijke commando variabelen van de store                            
resetCommands :: Store -> Store
resetCommands s = s { pressedN = False
                    , pressedR = False
                    , pressedE = False
                    , pressedD = False
                    , pressedW = False
                    , pressedF = False
                    , pressedQ = False
                    , pressedZ = False
                    , pressedX = False
                    , node1Select = Nothing
                    , node2Select = Nothing
                    }

-- | Start
--   Deze functie start het programma
--   De functie preEventloop start de server
--   en neemt de functie eventloop mee als handler
--   Ook wordt de begintoestand van de store meegegeven                    
start = preEventloop eventloop beginStore
                
-- | AddNode
--  Deze functie voegt de node toe aan de graaf
addNode :: Node -> Graph -> Graph
addNode n g@(Graph{nodes=ns}) = g {nodes=(n:ns)}

-- | AddEdge
-- Deze functie voegt de edge toe aan de graaf
addEdge :: Edge -> Graph -> Graph
addEdge e g@(Graph{edges=es}) = g {edges=(e:es)}

-- | Geeft de nodes terug die horen bij de edge
findNodesByEdge :: Edge -> Graph -> (Node, Node)
findNodesByEdge (l1, l2, _, _) graph = (n1, n2)
                                    where
                                        Just n1 = findNode l1 graph
                                        Just n2 = findNode l2 graph

-- | Geeft de node terug horend bij het label
findNode :: Label -> Graph -> Maybe Node
findNode l (Graph {nodes=nodes}) | null n = Nothing
                                 | otherwise = Just (head n)
                                where
                                    n = filter (\(nl, _, _) -> l == nl) nodes

-- | Geeft de edge terug horend bij de twee labels
findEdge :: Label -> Label -> Graph -> Maybe Edge
findEdge l1 l2 (Graph {edges=es}) | null e    = Nothing
                                  | otherwise = Just (head e)
                                  where
                                    e = filter (\(el1, el2, _, _) -> l1 == el1 && l2 == el2) es

-- | Geeft alle edges terug die van of naar de node gaan
--   die bijbehorend label heeft                                    
findEdgesSingleLabel :: Label -> Graph -> [Edge]
findEdgesSingleLabel l (Graph{edges=es}) = filter (\(el1, el2, _, _) -> el1 == l || el2 == l) es

-- | Verwijdert de node horend bij het label uit de graaf
removeNode :: Label -> Graph -> Graph
removeNode l g@(Graph{nodes=ns}) | nM == Nothing = g 
                                 | otherwise     = g {nodes = ns'}
                                where
                                    nM = findNode l g
                                    n  = fromJust nM
                                    ns' = delete n ns

-- | Verwijdert de edge horend bij de twee labels uit
--   de graaf
removeEdge :: Label -> Label -> Graph -> Graph
removeEdge l1 l2 g@(Graph{edges=es}) | eM == Nothing = g 
                                     | otherwise     = g {edges = es'}
                                    where
                                        eM  = findEdge l1 l2 g
                                        e   = fromJust eM
                                        es' = delete e es

-- | Verwijdert alle edges die van of naar de node gaan
--   horend bij het gegeven label
removeEdgesSingleLabel :: Label -> Graph -> Graph
removeEdgesSingleLabel l g@(Graph{edges=es}) = g {edges = es'}
                            where
                                es' = es \\ (findEdgesSingleLabel l g)

-- | Geeft de grafische output om de node te maken
nodeToOutput :: Node -> GraphOutput
nodeToOutput (l, p, c) = NodeG l p c


-- | Geeft de grafische output om de edge te maken
--  Deze functie houdt rekening met de eigenschappen
--  van de graaf zoals gewicht en directie
edgeToOutput :: Graph -> Edge -> GraphOutput
edgeToOutput graph@(Graph {weighted=weighted, directed=directed}) e@(l1, l2, c, w) | weighted == Weighted  = WeightedLineG n1 n2 w c Thin directed
                                                                                   | otherwise             = LineG n1 n2 c Thin directed 
                                                                                   where
                                                                                       (n1, n2) = findNodesByEdge e graph
graphToOutput :: Graph -> [GraphOutput]
graphToOutput g@Graph {nodes=nodes, edges=edges} = map nodeToOutput nodes ++ map (edgeToOutput g) edges ++ [instructions]

-- | De Eventloop
--   Dit is het hart van het grafische IO programma.
--   Elk input dat van belang is wordt gemapt naar
--   de bijbehorende uitvoer.
eventloop :: Store -> GraphInput -> ([GraphOutput], Store)

-- @ start
eventloop s@(Store {graph=graph}) Start = (graphToOutput graph, s)

-- | Deze functie hernoemt een node wanneer alle 
--   benodigde variabelen zijn gezet
eventloop s@(Store { pressedR = True
                   , node1Select = Just n
                   , graph = g
                   }) (KeyPress l) = (output, s'{graph=graph'})
                                  where
                                      (output, graph') = renameNode (head l) n g
                                      s'               = resetCommands s

-- | Deze functie geeft een gewicht aan een edge
--   wanneer alle benodigde variabelen zijn gezet
eventloop s@(Store { pressedW    = True
                   , node1Select = Just n1
                   , node2Select = Just n2
                   , graph       = g
                   }) (KeyPress d) | isDigit (head d) = (output, s'{graph=g'})
                                   | otherwise        = ([], s)
                                   where
                                        s' = resetCommands s
                                        i  = read d
                                        (output, g') = weightEdge n1 n2 i g
                            

-- | Zet het bijbehorende commando bij de toetsaanslag                            
eventloop s (KeyPress "n") = ([], s' {pressedN = True})
                      where
                          s' = resetCommands s
                          
-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "r") = ([], s' {pressedR = True})
                      where
                          s' = resetCommands s
                          
-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "e") = ([], s' {pressedE = True})
                      where
                          s' = resetCommands s
                          
-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "d") = ([], s' {pressedD = True})
                      where
                          s' = resetCommands s
                          
-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "w") = ([], s' {pressedW = True})
                      where
                          s' = resetCommands s
                          
-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "f") = ([], s' {pressedF = True})
                      where
                          s' = resetCommands s                                          

-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "q") = ([], s' {pressedQ = True})
                      where
                          s' = resetCommands s                                          

-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "z") = ([], s' {pressedZ = True})
                      where
                          s' = resetCommands s                                          

-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "x") = ([], s' {pressedX = True})
                      where
                          s' = resetCommands s                                          

-- | Deze functie geeft het correcte gedrag bij een muisklik.
--   Afhankelijk van welke toetsaanslag al is ingedrukt en of er
--   op een nodige is geklikt, wordt een variabele gezet of de complete
--   Store gezet met resetCommands.
eventloop s@(Store pn pr pe pd pw pf pq pz px n1s n2s g) (MouseUp MLeft pos)  | pn && node == Nothing                    = (output1, s' {graph=graph1})
                                                                        | pd && node /= Nothing                    = (output2, s' {graph=graph2})
                                                                        | pr && n1s  == Nothing                    = ([], s{node1Select = node})
                                                                        | pe && n1s  == Nothing                    = ([], s{node1Select = node})
                                                                        | pe && n1s  /= Nothing && node /= Nothing = (output3, s' {graph=graph3})
                                                                        | pw && n1s == Nothing                     = ([], s{node1Select = node})
                                                                        | pw && n1s /= Nothing                     = ([], s{node2Select = node})
                                                                        | pf && n1s == Nothing                     = ([], s{node1Select = node})
                                                                        | pf && n1s /= Nothing                     = (output4, s' {graph=graph4})
                                                                        | pq && node /= Nothing                    = (output5, s' {graph=graph5})
                                                                        | pz && node /= Nothing                    = (output6, s' {graph=graph6})
                                                                        | px                                       = (output7, s' {graph=graph7})
                                                                        | otherwise                                = ([], s)
                                                                              where
                                                                                (output1, graph1) = insertNode pos g
                                                                                (output2, graph2) = deleteNode (fromJust node) g
                                                                                (output3, graph3) = insertEdge (fromJust n1s) (fromJust node) g
                                                                                (output4, graph4) = deleteEdge (fromJust n1s) (fromJust node) g
                                                                                (output5, graph5) = recolorNode (fromJust node) Red g
                                                                                (output6, graph6) = colorAdjacentNodes (fromJust node) Blue g
                                                                                (output7, graph7) = colorAllNodes Black g
                                                                                node              = onNode (nodes g) pos
                                                                                s' = resetCommands s

-- | Voor alle andere uitvoer hoeft er niks te gebeuren                                                                                
eventloop s _ = ([], s)  

-- | Deze functie insert een node in de graaf op de bijbehorende
--   positie en geeft het een lege label. Ook wordt de grafische
--   uitvoer gegeven.
insertNode :: Pos -> Graph -> ([GraphOutput], Graph)
insertNode pos g@(Graph {nodes=ns}) | nM == Nothing = (output, g')
                                    | otherwise     = ([], g)
                                    where
                                        l = ' '
                                        nM = findNode l g
                                        n = (l, pos, Black)
                                        g' = addNode n g
                                        output = [RemoveNodeG l, nodeToOutput n]

-- | Deze functie verwijdert een node uit de graaf. Ook wordt de
--   grafische uitvoer gegeven.
deleteNode :: Node -> Graph -> ([GraphOutput], Graph)
deleteNode (l, _, _) g = (output, g'')
                    where
                        g' = removeNode l g
                        edgesFromNode = findEdgesSingleLabel l g'
                        g'' = removeEdgesSingleLabel l g'
                        output = RemoveNodeG l:(map (\(el1, el2, _, _)-> RemoveEdgeG el1 el2) edgesFromNode)

-- | Deze functie insert een edge in de graaf.
--   Ook wordt de grafische uitvoer gegeven.
insertEdge :: Node -> Node -> Graph -> ([GraphOutput], Graph)
insertEdge n1@(l1,_,_) n2@(l2,_,_) g@(Graph {directed=d}) | eM == Nothing = ([output], g')
                                                          | otherwise     = ([], g)
                                                          where
                                                              eM = findEdge l1 l2 g
                                                              e = (l1, l2, Black, -1)
                                                              g' = addEdge e g
                                                              output = LineG n1 n2 Black Thin d

-- | Hernoemt een node en update alle bijbehorende edges
--   Geeft ook de bijbehorende grafische uitvoer
renameNode :: Label -> Node -> Graph -> ([GraphOutput], Graph)
renameNode l' (l, p, c) g = (outN ++ outEs, g''')
                        where
                            g'           = removeNode l g
                            n'           = (l', p, c)
                            g''          = addNode n' g'
                            outN         = [RemoveNodeG l, nodeToOutput n']
                            (outEs, g''') = renameEdges l l' g''

-- | Hernoemt alle edges horend tot de oldL. Alle oldL wordt
--   vervangen door l. Ook geeft deze functie de bijbehorende
--   grafische uitvoer.
renameEdges :: Label -> Label -> Graph -> ([GraphOutput], Graph)
renameEdges oldL l g = (output, g')
                    where
                        es = findEdgesSingleLabel oldL g
                        (output, g') = foldl renEs ([],g) es
                        renEs (o, graph) e = let
                                              (o', graph') = renE e graph
                                              in (o ++ o', graph')
                        renE e graph = renameEdge oldL l e graph
 

-- | Hernoemt een enkele edge 
renameEdge :: Label -> Label -> Edge -> Graph -> ([GraphOutput], Graph)
renameEdge oldL l e@(el1, el2, c, w) g@(Graph{weighted=wei, directed = d}) | el1 == oldL && el2 == oldL = ([rem, edgeToOutput g' e1], addEdge e1 g')
                                                                           | el1 == oldL                = ([rem, edgeToOutput g' e2], addEdge e2 g')
                                                                           | el2 == oldL                = ([rem, edgeToOutput g' e3], addEdge e3 g')
                                                                    where
                                                                        (n1, n2)  = findNodesByEdge e g
                                                                        rem       = RemoveEdgeG el1 el2
                                                                        g'        = removeEdge el1 el2 g
                                                                        e1        = (l, l, c, w)
                                                                        e2        = (l, el2, c, w)
                                                                        e3        = (el1, l, c, w)                               

-- | Verandert het gewicht van de edge van node 1 naar node 2.                               
weightEdge :: Node -> Node -> Int -> Graph -> ([GraphOutput], Graph)
weightEdge n1@(l1, _, _) n2@(l2, _, _) w g | eM == Nothing = ([], g)
                                           | eM /= Nothing = ([rem, edgeToOutput g'' e'], g'')
                                          where
                                             rem          = RemoveEdgeG l1 l2
                                             eM           = findEdge l1 l2 g
                                             (_, _, c, _) = fromJust eM
                                             e'           = (l1, l2, c, w)
                                             g'           = removeEdge l1 l2 g
                                             g''          = addEdge e' g'

-- | Verwijdert de edge van node 1 naar node 2.
deleteEdge :: Node -> Node -> Graph -> ([GraphOutput], Graph)
deleteEdge (l1, _, _) (l2, _, _) g = ([RemoveEdgeG l1 l2], g')
                                    where
                                        g' = removeEdge l1 l2 g

-- | Geeft een node een nieuwe kleur
recolorNode :: Node -> ColorG -> Graph -> ([GraphOutput], Graph)
recolorNode (l, p, cOld) c g = ([nodeToOutput (l, p, c)], g') where
  g' = colorNodeInGraph (l, p, cOld) c g

-- | Kleurt aanliggende nodes
colorAdjacentNodes :: Node -> ColorG -> Graph -> ([GraphOutput], Graph)
colorAdjacentNodes n c g = (map nodeToOutput recoloredNodes, g')
  where
    nodes = findAdjacentNodes n g
    recoloredNodes = map (\(l, p, _) -> (l, p, c)) nodes
    g' = colorNodesInGraph nodes c g

-- | Kleurt alle nodes
colorAllNodes :: ColorG -> Graph -> ([GraphOutput], Graph)
colorAllNodes c g@Graph {nodes=nodes} = (map nodeToOutput recoloredNodes, g')
  where
    recoloredNodes = map (\(l, p, _) -> (l, p, c)) nodes
    g' = colorNodesInGraph nodes c g

nodeToLabel :: Node -> Label
nodeToLabel (l, _, _) = l

colorNodeInGraph :: Node -> ColorG -> Graph -> Graph
colorNodeInGraph (l, p, _) c g = g'' where
  g' = removeNode l g
  g'' = addNode (l, p, c) g'

colorNodesInGraph :: [Node] -> ColorG -> Graph -> Graph
colorNodesInGraph [] c g = g
colorNodesInGraph [n] c g = colorNodeInGraph n c g
colorNodesInGraph (n:ns) c g = (colorNodesInGraph ns c g')
  where
    g' = (colorNodeInGraph n c g)

findDirectedEdgesSingleLabel :: Label -> Graph -> [Edge]
findDirectedEdgesSingleLabel l (Graph{edges=es}) = filter (\(el1, _, _, _) -> el1 == l) es

findAdjacentNodes :: Node -> Graph -> [Node]
findAdjacentNodes n g@Graph {directed=Directed} = map fromJust nodes where
  adjacentEdges = findDirectedEdgesSingleLabel (nodeToLabel n) g
  adjacentLabels = map (\(_, endNode, _, _) -> endNode) adjacentEdges
  nodes = map (flip (findNode) g) adjacentLabels
findAdjacentNodes n g@Graph{directed=Undirected} = map fromJust (nodes) where
  adjacentEdges = findEdgesSingleLabel (nodeToLabel n) g
  beginLabels = map (\(beginLabel, _, _, _) -> beginLabel) adjacentEdges
  endLabels = map (\(_, endLabel, _, _) -> endLabel) adjacentEdges
  adjacentLabels = filter (/= nodeToLabel n) (beginLabels ++ endLabels)
  nodes = map (flip (findNode) g) adjacentLabels

isCompleted :: Graph -> Bool
isCompleted g@Graph{nodes=nodes} = and (map (flip isCompletedNode g) nodes)

isCompletedNode :: Node -> Graph -> Bool
isCompletedNode n g@Graph{nodes=nodes} = and (map (flip elem adjacentNodes) otherNodes)
  where
  adjacentNodes = findAdjacentNodes n g
  otherNodes = (\\) nodes [n]

isConnected :: Graph -> Bool
isConnected g@Graph{nodes=nodes} = and (map (flip elem allConnectedNodes) nodes)
  where
    allConnectedNodes = findSubgraph (head nodes) g

findSubgraph :: Node -> Graph -> [Node]
findSubgraph n g = [n] ++ findSubgraph' [] g n 

findSubgraph' :: [Node] -> Graph -> Node -> [Node]
findSubgraph' visitedNodes g n = neighbours' ++ concat (map (findSubgraph' visitedNodes' g) neighbours')
  where
    neighbours = findAdjacentNodes n g
    neighbours' = (\\) neighbours visitedNodes
    visitedNodes' = (n:visitedNodes)

findSubgraphs :: Graph -> [[Node]]
findSubgraphs g@Graph{nodes=nodes} = take (length subgraphs - 1) subgraphs
  where
    subgraphs = findSubgraphs' [] nodes g

findSubgraphs' :: [Node] -> [Node] -> Graph -> [[Node]]
findSubgraphs' visitedNodes [] g = [[]]
findSubgraphs' visitedNodes remainingNodes g = [subgraph] ++ findSubgraphs' visitedNodes' remainingNodes' g
  where
    subgraph = findSubgraph (head remainingNodes) g
    visitedNodes' = subgraph ++ visitedNodes
    remainingNodes' = (\\) remainingNodes visitedNodes'
