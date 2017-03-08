module Data.Graph
  ( AdjacencyList
  , Graph
  , fromAdjacencyList
  , empty
  , isEmpty
  , insertVertex
  , updateVertex
  , deleteVertex
  , insertEdge
  , deleteEdge
  , keys
  , vertices
  , size
  , lookup
  , member
  , adjacent
  , adjacent'
  , isAdjacent
  , weight
  , shortestPath
  , shortestPath'
  , traverse
  , connectedComponents
  , filter
  , update
  ) where

import Prelude

import Data.Foldable (elem, foldl) as F
import Data.List (List(..), (\\), (:))
import Data.List (filter, reverse, singleton, snoc) as L
import Data.Map (Map)
import Data.Map (alter, delete, empty, insert, isEmpty, keys, lookup, member, singleton, size, toList, update, values) as M
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Set (Set)
import Data.Set (empty, insert, member) as S
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

import Data.PQueue (PQueue)
import Data.PQueue (insert, isEmpty, singleton) as PQ
import Data.PQueue.Partial (head, tail) as PPQ

-- | `Graph k w v` represents a graph of vertices of type `v` with keys of
-- | type `k`, connected by edges with a weight of type `w`.
data Graph k w v = Graph (Map k v) (Map k (Map k w))

-- | `AdjacencyList k v w` represents a `List` of vertices of type `k` with a
-- | list of adjacent vertices connected with edges of type `w`.
type AdjacencyList k v w = List (Tuple k (Tuple v (List (Tuple k w))))

instance showGraph :: (Show k, Show v, Show w) => Show (Graph k w v) where
  show (Graph vertexMap edgeMap) = "Graph vertices: " <> show vertexMap <> " edges: " <> show edgeMap

instance functorGraph :: Functor (Graph k w) where
  map f (Graph vertexMap edgeMap) = Graph (map f vertexMap) edgeMap

-- | Create a graph from an adjacency list.
fromAdjacencyList :: forall k v w. Ord k => AdjacencyList k v w -> Graph k w v
fromAdjacencyList as = (insertEdges <<< insertVertices) empty
  where
    -- Unwrap the vertices from the adjacency list and insert them into the graph.
    insertVertices :: Graph k w v -> Graph k w v
    insertVertices g = F.foldl (flip unwrapVertex) g as

    unwrapVertex :: Tuple k (Tuple v (List (Tuple k w))) -> Graph k w v -> Graph k w v
    unwrapVertex (Tuple k (Tuple v _)) = insertVertex k v

    -- Unwrap the edges from the adjacency list and insert them into the graph.
    insertEdges :: Graph k w v -> Graph k w v
    insertEdges g = F.foldl (flip unwrapEdges) g as

    unwrapEdges :: Tuple k (Tuple v (List (Tuple k w))) -> Graph k w v -> Graph k w v
    unwrapEdges (Tuple a (Tuple v edges)) g = F.foldl (flip $ unwrapEdge a) g edges

    unwrapEdge :: k -> (Tuple k w) -> Graph k w v -> Graph k w v
    unwrapEdge a (Tuple b w) = insertEdge a b w

-- | An empty graph.
empty :: forall k v w. Graph k w v
empty = Graph M.empty M.empty

-- | Test whether a graph is empty.
isEmpty :: forall k v w. Ord k => Graph k w v -> Boolean
isEmpty (Graph vertexMap _) = M.isEmpty vertexMap

-- | Insert a vertex into a graph.
insertVertex :: forall k v w. Ord k => k -> v -> Graph k w v -> Graph k w v
insertVertex key value (Graph vertexMap edgeMap) = Graph (M.insert key value vertexMap) edgeMap

-- | Update a vertex in a graph.
updateVertex :: forall k v w. Ord k => k -> v -> Graph k w v -> Graph k w v
updateVertex key value (Graph vertexMap edgeMap) = Graph (M.update updateVertex' key vertexMap) edgeMap
  where updateVertex' _ = Just value

-- | Delete a vertex from a graph.
deleteVertex :: forall k v w. Ord k => k -> Graph k w v -> Graph k w v
deleteVertex vertex = (deleteVertex' <<< deleteEdges <<< deleteIncidentEdges)
  where
    deleteVertex' (Graph vertexMap edgeMap) = Graph (M.delete vertex vertexMap) edgeMap
    deleteEdges (Graph vertexMap edgeMap) = Graph vertexMap (M.delete vertex edgeMap)
    deleteIncidentEdges graph = F.foldl (\g v -> deleteEdge v vertex g) graph (adjacent vertex graph)

-- | Insert an edge into a graph.
insertEdge :: forall k v w. Ord k => k -> k -> w -> Graph k w v -> Graph k w v
insertEdge from to weight (Graph vertexMap edgeMap) = Graph vertexMap (M.alter insertEdge' from edgeMap)
  where insertEdge' = maybe (Just $ M.singleton to weight) (\weightMap -> Just $ M.insert to weight weightMap)

-- | Delete an edge from a graph.
deleteEdge :: forall k v w. Ord k => k -> k -> Graph k w v -> Graph k w v
deleteEdge from to (Graph vertexMap edgeMap) = Graph vertexMap (M.alter deleteEdge' from edgeMap)
  where deleteEdge' = maybe Nothing (\weightMap -> Just $ M.delete to weightMap)

-- | Get the keys of a graph.
keys :: forall k v w. Graph k w v -> List k
keys (Graph vertexMap _) = M.keys vertexMap

-- | Get the vertices of a graph.
vertices :: forall k v w. Graph k w v -> List v
vertices (Graph vertexMap _) = M.values vertexMap

-- | Get the number of vertices in a graph.
size :: forall k v w. Graph k w v -> Int
size (Graph vertexMap _) = M.size vertexMap

-- | Get a vertex matching a key.
lookup :: forall k v w. Ord k => k -> Graph k w v -> Maybe v
lookup key (Graph vertexMap _) = M.lookup key vertexMap

-- | Test whether a key is in a graph.
member :: forall k v w. Ord k => k -> Graph k w v -> Boolean
member key (Graph vertexMap _) = M.member key vertexMap

-- | Get the keys of the vertices adjacent to a key.
adjacent :: forall k v w. Ord k => k -> Graph k w v -> List k
adjacent key (Graph _ edgeMap)= maybe Nil M.keys (M.lookup key edgeMap)

-- | Get the adjacent vertices and associated costs of a vertex.
adjacent' :: forall k v w. Ord k => k -> Graph k w v -> List (Tuple k w)
adjacent' key (Graph _ edgeMap)= maybe Nil M.toList (M.lookup key edgeMap)

-- | Test whether two vertices are adjacent in a graph.
isAdjacent :: forall k v w. Ord k => k -> k -> Graph k w v -> Boolean
isAdjacent a b (Graph _ edgeMap) = maybe false (M.member b) (M.lookup a edgeMap)

-- | Get the weight of the edge between two vertices. Returns `Nothing` if no
-- | edge exists between the vertices.
weight :: forall k v w. Ord k => k -> k -> Graph k w v -> Maybe w
weight a b (Graph _ edgeMap) = maybe Nothing (M.lookup b) (M.lookup a edgeMap)

-- | Get the shortest path between two vertices. Returns `Nothing` if no path
-- | exists between the vertices.
shortestPath :: forall k v w. (Ord k, Ord w, Semiring w) => k -> k -> Graph k w v -> Maybe (List k)
shortestPath from to = shortestPath' (_ == to) from

-- | Get the shortest path from a starting vertex to a vertex that satisifes a
-- | predicate function. Returns `Nothing` if no path exists between the
-- | vertices.
shortestPath' :: forall k v w. (Ord k, Ord w, Semiring w) => (k -> Boolean) -> k -> Graph k w v -> Maybe (List k)
shortestPath' p start g = go (PQ.singleton zero start) S.empty (M.singleton start zero) M.empty
  where
    go :: PQueue w k     -- priority queue of fringe vertices
       -> Set k          -- set of visited vertices
       -> Map k w        -- map from vertices to costs
       -> Map k k        -- map from vertices to adjacent vertices
       -> Maybe (List k) -- shortest path
    go fringe visited labels edges =
      if PQ.isEmpty fringe then Nothing
      else
        let smallest = unsafePartial $ PPQ.head fringe
            cost = fst smallest
            vertex = snd smallest
            fringe' = unsafePartial $ PPQ.tail fringe
            visited' = S.insert vertex visited
            labels' = F.foldl (\a (Tuple v c) -> M.insert v c a) labels successors
            successors = L.filter isSuccessor $ successorsAndCosts vertex cost
            isSuccessor (Tuple v c) = not (S.member v visited') && ((not (M.member v labels)) || c < (lookup' v labels))
        in if p vertex then Just $ findPath vertex edges
           else if S.member vertex visited then go fringe' visited labels edges
           else
             let fringe'' = F.foldl (\a (Tuple v c) -> PQ.insert c v a) fringe' successors
                 edges' = F.foldl (\a (Tuple v _) -> M.insert v vertex a) edges successors
             in go fringe'' visited' labels' edges'

    successorsAndCosts :: k -> w -> List (Tuple k w)
    successorsAndCosts v cost = map (\(Tuple v c) -> Tuple v (cost + c)) (adjacent' v g)

    findPath :: k -> Map k k -> List k
    findPath vertex edges
      | M.member vertex edges =
        let vertex' = lookup' vertex edges
        in (findPath vertex' edges) <> L.singleton vertex
      | otherwise = L.singleton vertex

    lookup' :: forall k v. Ord k => k -> Map k v -> v
    lookup' k m = unsafePartial $ fromJust $ M.lookup k m

-- | Perform a depth-frist traversal of a graph from a starting vertex.
-- | Returns a `List` of the visited vertices.
traverse :: forall k v w. Ord k => k -> Graph k w v -> List k
traverse from g
  | member from g =
    let go Nil path = path
        go (v:vs) path
          | F.elem v path = go vs path
          | otherwise = go (adjacent v g <> vs) (v:path)
    in L.reverse $ go (L.singleton from) Nil
  | otherwise = Nil

-- | Get the strongly connected components of a graph. Returns a `List` of
-- | connected subgraphs.
connectedComponents :: forall k v w. Ord k => Graph k w v -> List (Graph k w v)
connectedComponents g =
  let go Nil gs = gs
      go (k:ks) gs =
        let ks' = traverse k g
            as = map (\a -> Tuple a (Tuple (unsafePartial $ fromJust $ lookup a g) (adjacent' a g))) ks'
        in go (ks \\ ks') (L.snoc gs (fromAdjacencyList as))
  in go (keys g) Nil

-- | Filter a graph, keeping the vertices which satisfy a predicate function.
filter :: forall k v w. Ord k => (v -> Boolean) -> Graph k w v -> Graph k w v
filter f graph = F.foldl filterVertex graph (keys graph)
  where
    filterVertex g k = if f (unsafePartial $ fromJust $ lookup k graph)
                         then g
                         else deleteVertex k g

-- | Update a vertex in a graph using a function.
update :: forall k v w. Ord k => (v -> Maybe v) -> k -> Graph k w v -> Graph k w v
update f key graph = maybe graph (g <<< f) vertex
  where
    vertex :: Maybe v
    vertex = lookup key graph

    g :: Maybe v -> Graph k w v
    g = maybe (deleteVertex key graph) (\v -> updateVertex key v graph)
