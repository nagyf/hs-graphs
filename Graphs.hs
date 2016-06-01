module Graphs where

import qualified Data.List as L

data Edge a = Edge { source :: a, destination :: a } deriving Show
data Graph a = Empty | Graph [Edge a] deriving Show

-- Adds an edge to the graph
infixl 7 <+>
(<+>) :: Graph a -> (a, a) -> Graph a
Empty <+> (x, y) = Graph [Edge x y]
(Graph cs) <+> (x, y) = Graph $ Edge x y:cs

adjacencyMatrix :: (Ord a, Eq a) => Graph a -> [[Int]]
adjacencyMatrix Empty = [[]]
adjacencyMatrix g =
    let vs = L.sort $ vertices g
    in [map fromBool [connected g x y | y <- vs] | x <- vs]

    where
        fromBool :: Bool -> Int
        fromBool True = 1
        fromBool False = 0

-- List all vertices in a graph
vertices :: (Eq a) => Graph a -> [a]
vertices Empty = []
vertices (Graph cs) = L.nub $ map source cs ++ map destination cs

-- Find all edges related to a vertex
find :: (Eq a) => Graph a -> a -> [Edge a]
find Empty _ = []
find (Graph cs) x = filter (\c -> source c == x || destination c == x) cs

-- Return true if the 2 vertices are connected
connected :: (Eq a) => Graph a -> a -> a -> Bool
connected Empty _ _ = False
connected g x y = any (\(Edge u v) -> u == y || v == y) $ find g x

-- Return the degree of a vertex
degree :: (Eq a) => Graph a -> a -> Int
degree g x = length $ find g x

-- Convert a list of elements to a list of tuples
tuples :: [a] -> [(a, a)]
tuples [] = []
tuples [_] = error "Unable to construct tuple list"
tuples (x:y:xs) = (x,y):tuples xs

-- Load a graph from file
loadFromFile :: FilePath -> IO (Graph Int)
loadFromFile p = do
    content <- lines <$> readFile p
    let edges = tuples $ map read $ concatMap words content
    return $ foldl (<+>) Empty edges

main :: IO ()
main = do
    g <- loadFromFile "connections.txt"
    let vs = L.sort $ vertices g
    mapM_ (putStrLn . whatDegree g) vs
    mapM_ print (adjacencyMatrix g)

    where
        whatDegree g v =
            "Node " ++ show v ++ " has a degree of: " ++ show (degree g v)