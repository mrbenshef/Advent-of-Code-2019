{-#  LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8    as BS
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import           Safe                           ( headMay )

main = do
  orbits <- map (BS.split ')') . BS.lines <$> BS.readFile "input"
  let orbitMap = Map.fromListWith (++) (map (\[a, b] -> (a, [b])) orbits)
  print $ sum [ countOrbits orbitMap name | name <- Map.keys orbitMap ]
  print $ minimum $ Maybe.catMaybes
    [ let distance t = distanceTo orbitMap t name
      in  ((+) <$> distance "YOU" <*> distance "SAN")
    | name <- Map.keys orbitMap
    ]

countOrbits orbitMap name = case orbitMap Map.!? name of
  Just names -> length names + sum (map (countOrbits orbitMap) names)
  Nothing    -> 0

distanceTo orbitMap target name = case orbitMap Map.!? name of
  Just names -> if target `elem` names
    then Just 0
    else (+ 1) <$> headMay (Maybe.mapMaybe (distanceTo orbitMap target) names)
  Nothing -> Nothing
