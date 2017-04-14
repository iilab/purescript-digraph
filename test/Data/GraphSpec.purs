module Test.Data.GraphSpec where

import Prelude

import Data.Array (elem) as A
import Control.Monad.Aff (Aff)
import Data.Char (toLower) as C
import Data.Foldable (class Foldable)
import Data.List ((!!))
import Data.List (fromFoldable) as L
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Graph

shouldMatch :: forall a f g r. Show a => Eq a => Foldable f => Foldable g => f a -> g a -> Aff r Unit
shouldMatch a b = shouldEqual (L.fromFoldable a) (L.fromFoldable b)

graphSpec :: forall r. (Spec r) Unit
graphSpec = describe "Graph" do
  let vertexList = [ Tuple 1 'A'
                   , Tuple 2 'B'
                   , Tuple 3 'C'
                   , Tuple 4 'D'
                   , Tuple 5 'E'
                   , Tuple 6 'F'
                   , Tuple 7 'G'
                   ]

  let edgeList = [ Tuple 1 [Tuple 2 1, Tuple 3 2]
                 , Tuple 2 [Tuple 1 1, Tuple 4 3]
                 , Tuple 3 [Tuple 1 2, Tuple 4 4]
                 , Tuple 4 [Tuple 2 3, Tuple 3 4]
                 , Tuple 5 [Tuple 6 1]
                 , Tuple 6 [Tuple 5 1]
                 , Tuple 7 []
                 ]

  let graph = fromFoldable vertexList edgeList

  describe "isEmpty" do
    it "returns true if the graph is empty" do
      isEmpty (empty :: Graph Int Int Int) `shouldEqual` true

  describe "vertices" do
    it "returns the vertices" do
      vertices graph `shouldMatch` ['A', 'B', 'C', 'D', 'E', 'F', 'G']

  describe "size" do
    it "returns the number of vertices" do
      size graph `shouldEqual` 7

  describe "lookup" do
    it "returns true if the graph contains a vertex" do
      lookup 1 graph `shouldEqual` Just 'A'

    it "returns false if the graph does not contain a vertex" do
      lookup 8 graph `shouldEqual` Nothing

  describe "member" do
    it "returns true if the graph contains a vertex" do
      member 1 graph `shouldEqual` true

    it "returns false if the graph does not contain a vertex" do
      member 8 graph `shouldEqual` false

  describe "adjacent" do
    it "returns the adjacent vertices given a vertex in the graph" do
      adjacent 1 graph `shouldMatch` [2, 3]
      adjacent 2 graph `shouldMatch` [1, 4]
      adjacent 3 graph `shouldMatch` [1, 4]
      adjacent 4 graph `shouldMatch` [2, 3]
      adjacent 5 graph `shouldMatch` [6]
      adjacent 6 graph `shouldMatch` [5]

    it "returns an empty list given a vertex with no edges" do
      adjacent 7 graph `shouldMatch` []

    it "returns an empty list given a vertex not in the graph" do
      adjacent 8 graph `shouldMatch` []

  describe "isAdjacent" do
    it "returns true if the vertices are adjacent" do
      isAdjacent 1 2 graph `shouldEqual` true

    it "returns false if the vertices are not adjacent" do
      isAdjacent 1 4 graph `shouldEqual` false

  describe "weight" do
    it "returns the weight of the edge between the vertices" do
      weight 1 2 graph `shouldEqual` Just 1
      weight 1 3 graph `shouldEqual` Just 2
      weight 2 4 graph `shouldEqual` Just 3
      weight 3 4 graph `shouldEqual` Just 4

    it "returns nothing if there is no edge between the vertices" do
      weight 1 4 graph `shouldEqual` Nothing

  describe "shortestPath" do
    it "returns the shortest path between the vertices" do
      shortestPath 1 1 graph `shouldEqual` Just (L.fromFoldable [1])
      shortestPath 1 2 graph `shouldEqual` Just (L.fromFoldable [1, 2])
      shortestPath 1 3 graph `shouldEqual` Just (L.fromFoldable [1, 3])
      shortestPath 1 4 graph `shouldEqual` Just (L.fromFoldable [1, 2, 4])
      shortestPath 4 1 graph `shouldEqual` Just (L.fromFoldable [4, 2, 1])
      shortestPath 4 2 graph `shouldEqual` Just (L.fromFoldable [4, 2])
      shortestPath 4 3 graph `shouldEqual` Just (L.fromFoldable [4, 3])
      shortestPath 4 4 graph `shouldEqual` Just (L.fromFoldable [4])

    it "returns nothing if there is no shortest path between the vertices" do
      shortestPath 1 5 graph `shouldEqual` Nothing

  describe "traverse" do
    it "returns the vertices visited in a depth-first traversal of the graph" do
      traverse 1 graph `shouldMatch` [1, 2, 4, 3]
      traverse 2 graph `shouldMatch` [2, 1, 3, 4]
      traverse 3 graph `shouldMatch` [3, 1, 2, 4]
      traverse 4 graph `shouldMatch` [4, 2, 1, 3]
      traverse 5 graph `shouldMatch` [5, 6]
      traverse 6 graph `shouldMatch` [6, 5]
      traverse 7 graph `shouldMatch` [7]

    it "returns an empty list given a vertex not in the graph" do
      traverse 8 graph `shouldMatch` []

  describe "connectedComponents" do
    it "returns the connected components of the graph" do
      let components = connectedComponents graph
      keys (unsafePartial $ fromJust $ components !! 0) `shouldMatch` [1, 2, 3, 4]
      keys (unsafePartial $ fromJust $ components !! 1) `shouldMatch` [5, 6]
      keys (unsafePartial $ fromJust $ components !! 2) `shouldMatch` [7]

  describe "insertVertex" do
    it "inserts a vertex into the graph" do
      vertices (insertVertex 8 'H' graph) `shouldMatch` ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']

  describe "insertEdge" do
    it "inserts an edge into the graph" do
      let graph' = insertEdge 6 7 5 graph
      weight 6 7 graph' `shouldEqual` Just 5

  describe "deleteVertex" do
    it "deletes a vertex from the graph" do
      keys (deleteVertex 1 graph) `shouldMatch` [2, 3, 4, 5, 6, 7]

    it "returns the graph unchanged if there is no matching vertex" do
      keys (deleteVertex 8 graph) `shouldMatch` [1, 2, 3, 4, 5, 6, 7]

    it "deletes the edges incident on a vertex from the graph" do
      let graph' = deleteVertex 1 graph
      isAdjacent 1 2 graph' `shouldEqual` false
      isAdjacent 2 1 graph' `shouldEqual` false

  describe "deleteEdge" do
    it "deletes an edge from the graph" do
      let graph' = deleteEdge 1 2 graph
      isAdjacent 1 2 graph' `shouldEqual` false
      isAdjacent 2 1 graph' `shouldEqual` true

    it "returns the graph unchanged if there is no matching edge" do
      let graph' = deleteEdge 6 7 graph
      isAdjacent 6 7 graph' `shouldEqual` false

  describe "map" do
    it "maps a function over the vertices of the graph" do
      vertices (map C.toLower graph) `shouldMatch` ['a', 'b', 'c', 'd', 'e', 'f', 'g']

  describe "filter" do
    it "removes matching vertices of the graph" do
      let f v = A.elem v ['A', 'B', 'C']
      keys (filter f graph) `shouldMatch` [1, 2, 3]

  describe "update" do
    it "updates a vertex of the graph" do
      let f = const $ Just 'H'
      vertices (update f 1 graph) `shouldMatch` ['H', 'B', 'C', 'D', 'E', 'F', 'G']

    it "deletes the vertex if the function returns Nothing" do
      let f = const Nothing
      vertices (update f 1 graph) `shouldMatch` ['B', 'C', 'D', 'E', 'F', 'G']
