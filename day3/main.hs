{-#  LANGUAGE OverloadedStrings #-}

import           System.IO
import qualified Data.ByteString.Lazy.Char8    as BS
import           Control.Applicative

import qualified Data.List                     as List
import qualified Data.Set                      as Set

data Direction = R Int | D Int | U Int | L Int deriving (Show)

main = do
  paths <- BS.lines <$> BS.readFile "input"
  let [path1, path2] = map (map (parseDir . BS.unpack) . BS.split ',') paths
  print $ minIntersection path1 path2

parseDir :: String -> Direction
parseDir ('R' : r) = R (read r)
parseDir ('D' : d) = D (read d)
parseDir ('U' : u) = U (read u)
parseDir ('L' : l) = L (read l)

allCords :: (Int, Int, Int) -> [Direction] -> [(Int, Int, Int)]
allCords _ [] = []
allCords (x, y, s) (R r : ds) =
  [ (x + r', y, s + r') | r' <- [1 .. r] ] ++ allCords (x + r, y, s + r) ds
allCords (x, y, s) (D d : ds) =
  [ (x, y - d', s + d') | d' <- [1 .. d] ] ++ allCords (x, y - d, s + d) ds
allCords (x, y, s) (U u : ds) =
  [ (x, y + u', s + u') | u' <- [1 .. u] ] ++ allCords (x, y + u, s + u) ds
allCords (x, y, s) (L l : ds) =
  [ (x - l', y, s + l') | l' <- [1 .. l] ] ++ allCords (x - l, y, s + l) ds

minIntersection :: [Direction] -> [Direction] -> Int
minIntersection line1 line2 =
  Set.findMin
    $                  Set.map (\(x, y) -> abs x + abs y)
    $                  dt1 ls1
    `Set.intersection` dt1 ls2
 where
  dt1 = Set.map (\(a, b, c) -> (a, b))
  ls1 = Set.fromList (allCords (0, 0, 0) line1)
  ls2 = Set.fromList (allCords (0, 0, 0) line2)
