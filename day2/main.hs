{-#  LANGUAGE OverloadedStrings #-}
import           System.IO
import qualified Data.ByteString.Lazy.Char8    as BS
import           Control.Applicative
import           Data.Maybe

data Op
    = BinOp (Int -> Int -> Int) Int Int Int
    | Halt

decode :: [Int] -> Maybe Op
decode (99                  : _) = Just Halt
decode (1 : in1 : in2 : out : _) = Just $ BinOp (+) in1 in2 out
decode (2 : in1 : in2 : out : _) = Just $ BinOp (*) in1 in2 out
decode _                         = Nothing

execOp :: Op -> [Int] -> [Int]
execOp (BinOp op in1 in2 out) tape =
  let result             = (tape !! in1) `op` (tape !! in2)
      (ltape, _ : rtape) = splitAt out tape
  in  ltape ++ [result] ++ rtape

exec :: Int -> [Int] -> [Int]
exec pointer program = case op of
  Nothing    -> error "invalid op"
  Just Halt  -> program
  Just binOp -> exec (pointer + 4) (execOp binOp program)
  where op = decode $ drop pointer program

setInput :: Int -> Int -> [Int] -> [Int]
setInput noun verb (h : _ : _ : tape) = h : noun : verb : tape

part1 tape = head $ exec 0 $ setInput 12 2 tape

part2 tape = 100 * noun + verb
 where
  (noun, verb) = head
    [ (noun, verb)
    | noun <- [0 .. 99]
    , verb <- [0 .. 99]
    , head (exec 0 (setInput noun verb tape)) == 19690720
    ]

main = do
  tape <- map readInt . BS.split ',' <$> BS.readFile "input"
  case sequence tape of
    Just tape ->
      putStrLn $ "part 1: " ++ show (part1 tape) ++ "\npart 2: " ++ show
        (part2 tape)
    Nothing -> putStrLn "invalid input"
  where readInt = fmap fst . BS.readInt
