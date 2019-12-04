{-#  LANGUAGE OverloadedStrings #-}

import           System.IO
import qualified Data.ByteString.Lazy.Char8    as BS
import           Control.Applicative

import qualified Data.List                     as List
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )

data Direction = R Int | D Int | U Int | L Int deriving (Show)

main = do
  paths <- BS.lines <$> BS.readFile "input"
  let [path1, path2] = map (map (parseDir . BS.unpack) . BS.split ',') paths
  print $ minIntersection path1 path2
  print $ minSteps path1 path2

parseDir :: String -> Direction
parseDir ('R' : r) = R (read r)
parseDir ('D' : d) = D (read d)
parseDir ('U' : u) = U (read u)
parseDir ('L' : l) = L (read l)

allCords :: (Int, Int, Int) -> [Direction] -> Map (Int, Int) Int
allCords _ [] = Map.empty
allCords (x, y, s) (R r : ds) =
  Map.fromList [ ((x + r', y), s + r') | r' <- [1 .. r] ]
    `Map.union` allCords (x + r, y, s + r) ds
allCords (x, y, s) (D d : ds) =
  Map.fromList [ ((x, y - d'), s + d') | d' <- [1 .. d] ]
    `Map.union` allCords (x, y - d, s + d) ds
allCords (x, y, s) (U u : ds) =
  Map.fromList [ ((x, y + u'), s + u') | u' <- [1 .. u] ]
    `Map.union` allCords (x, y + u, s + u) ds
allCords (x, y, s) (L l : ds) =
  Map.fromList [ ((x - l', y), s + l') | l' <- [1 .. l] ]
    `Map.union` allCords (x - l, y, s + l) ds

minIntersection :: [Direction] -> [Direction] -> Int
minIntersection path1 path2 =
  List.minimum
    $                  map (\(x, y) -> abs x + abs y)
    $                  Map.keys
    $                  allCords (0, 0, 0) path1
    `Map.intersection` allCords (0, 0, 0) path2

minSteps :: [Direction] -> [Direction] -> Int
minSteps path1 path2 = List.minimum $ Map.elems $ Map.intersectionWith
  (+)
  (allCords (0, 0, 0) path1)
  (allCords (0, 0, 0) path2)
