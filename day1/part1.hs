{-#  LANGUAGE OverloadedStrings #-}
import           System.IO
import qualified Data.ByteString.Lazy.Char8    as BS
import           Control.Applicative
import           Data.Maybe

main = do
  modules <- map readInt . BS.lines <$> BS.readFile "input"
  case sequence modules of
    Just masses -> print $ sum $ map fule masses
    Nothing     -> putStrLn "input invalid"
 where
  readInt = fmap fst . BS.readInt
  fule mass = mass `div` 3 - 2
