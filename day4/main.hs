import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.List                     as List

main = do
  range <- BS.split '-' <$> BS.readFile "input"
  let [Just min, Just max] = map (fmap fst . BS.readInt) range
  print $ solve (>= 2) min max
  print $ solve (== 2) min max

solve :: (Int -> Bool) -> Int -> Int -> Int
solve adj min max = length
  [ i
  | i <- [min .. max]
  , (isIncreasing . show) i
  , (any (adj . length) . List.group . show) i
  ]
 where
  isIncreasing (a : b : ls) = b >= a && isIncreasing (b : ls)
  isIncreasing _            = True
