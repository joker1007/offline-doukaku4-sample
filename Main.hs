module Main where

import Data.Set
import Data.Map

-- | U V W X Y
-- | P Q R S T
-- | K L M N O
-- | F G H I J
-- | A B C D E
data Node = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y deriving (Show, Eq, Ord, Enum)

data Path = Path {pathSource :: Node, pathTarget :: Node} deriving (Eq)

instance Show Path where
    show a = show (pathSource a) ++ "-" ++ show (pathTarget a)

data Vertex = Vertex {vertexNode :: Node, getPaths :: [Path]}

instance Show Vertex where
    show a = "(" ++ show (vertexNode a) ++ "," ++ show (getPaths a) ++ ")"

instance Eq Vertex where
    (==) a b = (vertexNode a) == (vertexNode b)

data Step = Step { currentVertex :: Vertex, getSteps :: [Vertex], visitted :: Set Vertex } deriving (Show)

data Graph = Graph { vertexMap :: Map Node Vertex } deriving (Show)

-- |
-- >>> createVertex A
-- (A,[A-B,A-F])
--
-- >>> createVertex B
-- (B,[B-A,B-C,B-G])
createVertex :: Node -> Vertex
createVertex n = Vertex n $ paths
  where
    paths = Prelude.map (\n2 -> Path (fst $ table n) n2 ) (snd $ table n)
    succ5 :: (Enum a) => a -> a
    succ5 = succ . succ . succ . succ . succ

    pred5 :: (Enum a) => a -> a
    pred5 = pred . pred . pred . pred . pred

    table node
      | node == A = (node, [succ node, succ5 node])
      | node == E = (node, [pred node, succ5 node])
      | node == U = (node, [pred5 node, succ node])
      | node == Y = (node, [pred5 node, pred node])
      | elem node [B,C,D] = (node, [pred node, succ node, succ5 node])
      | elem node [G,H,I,L,M,N,Q,R,S] = (node, [pred5 node, pred node, succ node, succ5 node])
      | elem node [F,K,P] = (node, [pred5 node, succ node, succ5 node])
      | elem node [J,O,T] = (node, [pred5 node, pred node, succ5 node])
      | elem node [V,W,X] = (node, [pred5 node, pred node, succ node])
      | otherwise = error "should not here"

main :: IO ()
main = undefined
