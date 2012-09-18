module Main where

import Data.Set
import Data.Char
import Control.Monad
import System.Environment
import Control.Applicative ((<$>))

-- | U V W X Y
-- | P Q R S T
-- | K L M N O
-- | F G H I J
-- | A B C D E
data Node = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y deriving (Show, Eq, Ord, Enum, Read)

data Path = Path {pathSource :: Node, pathTarget :: Node} deriving (Eq)

instance Show Path where
    show a = show (pathSource a) ++ "-" ++ show (pathTarget a)

data Vertex = Vertex {vertexNode :: Node, getPaths :: [Path]}

instance Ord Vertex where
    compare a b = vertexNode a `compare` vertexNode b

instance Show Vertex where
    show a = "(" ++ show (vertexNode a) ++ "," ++ show (getPaths a) ++ ")"

instance Eq Vertex where
    (==) a b = (vertexNode a) == (vertexNode b)

data Step = Step { currentVertex :: Vertex, getSteps :: [Vertex], visitted :: Set Vertex }

instance Show Step where
    show a = show $ Prelude.map vertexNode $ getSteps a

-- |
-- >>> parsePath "af"
-- [A-F]
--
-- >>> parsePath "af kl"
-- [A-F,K-L]
parsePath :: String -> [Path]
parsePath ss = Prelude.map p $ words ss
  where
    p (s:t:[]) = Path (read [toUpper s]) (read [toUpper t])
    p _ = error "should not be here"

-- |
-- >>> createVertex A []
-- (A,[A-B,A-F])
--
-- >>> createVertex A [Path A F]
-- (A,[A-B])
--
-- >>> createVertex B []
-- (B,[B-A,B-C,B-G])
--
-- >>> createVertex X [Path X Y, Path S X]
-- (X,[X-W])
createVertex :: Node -> [Path] -> Vertex
createVertex n ps = Vertex n $ paths
  where
    paths = Prelude.map (\n2 -> Path (fst $ table) n2 ) (snd $ table)
    succ5 :: (Enum a) => a -> a
    succ5 = succ . succ . succ . succ . succ

    pred5 :: (Enum a) => a -> a
    pred5 = pred . pred . pred . pred . pred

    table
      | n == A = (n, filterTable [succ n, succ5 n])
      | n == E = (n, filterTable [pred n, succ5 n])
      | n == U = (n, filterTable [pred5 n, succ n])
      | n == Y = (n, filterTable [pred5 n, pred n])
      | elem n [B,C,D] = (n, filterTable [pred n, succ n, succ5 n])
      | elem n [G,H,I,L,M,N,Q,R,S] = (n, filterTable [pred5 n, pred n, succ n, succ5 n])
      | elem n [F,K,P] = (n, filterTable [pred5 n, succ n, succ5 n])
      | elem n [J,O,T] = (n, filterTable [pred5 n, pred n, succ5 n])
      | elem n [V,W,X] = (n, filterTable [pred5 n, pred n, succ n])
      | otherwise = error "should not here"
    filterTable = Prelude.filter (\n2 -> notElem (Path n2 n) ps && notElem (Path n n2) ps)

-- |
-- >>> let s = initStep []
-- >>> length $ nextStep [] s
-- 8512
nextStep :: [Path] -> Step -> [()]
nextStep ps s = do p <- getPaths $ currentVertex s
                   v <- return $ createVertex (pathTarget p) ps
                   guard $ Data.Set.notMember v $ visitted s
                   ns <- return $ Step v (v:getSteps s) (Data.Set.insert v $ visitted s)
                   if vertexNode v == Y
                     then do return ()
                     else nextStep ps ns

initStep :: [Path] -> Step
initStep ps = let a = createVertex A ps
              in Step a [a] $ Data.Set.fromList [a]

main :: IO ()
main = do stopPaths <- parsePath <$> liftM head getArgs
          s <- return $ nextStep stopPaths $ initStep stopPaths
          putStrLn $ show $ length $ s
