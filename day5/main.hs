{-#  LANGUAGE OverloadedStrings #-}
{-#  LANGUAGE TypeApplications #-}
import           System.IO
import qualified Data.ByteString.Lazy.Char8    as BS
import           Control.Applicative
import           Data.Maybe

data Parameter
    = Immediate Int
    | Pointer Int
    deriving (Show)

data Op
    = Add
    | Mul
    | Input
    | Output
    | JumpT
    | JumpF
    | Lt
    | Eq
    deriving (Show, Enum)

data Instruction = Instruction Op [Parameter] deriving (Show)

opSize :: Op -> Int
opSize Add    = 4
opSize Mul    = 4
opSize Input  = 2
opSize Output = 2
opSize JumpT  = 3
opSize JumpF  = 3
opSize Lt     = 4
opSize Eq     = 4

padLeft :: a -> Int -> [a] -> [a]
padLeft c n xs = replicate (n - length xs) c ++ xs

decode :: [String] -> Instruction
decode (s : ss) = Instruction
  op
  [ decodeP (modes' !! (pCount - 1 - i)) (read $ ss !! i)
  | i <- [0 .. (pCount - 1)]
  ]
 where
  (modes, opCode) = splitAt (length s - 2) s
  op              = (toEnum . subtract 1 . read @Int) opCode
  modes'          = padLeft '0' pCount modes
  pCount          = opSize op - 1
  decodeP mode | mode == '0' = Pointer
               | mode == '1' = Immediate

replaceAt :: a -> Int -> [a] -> [a]
replaceAt elem idx ls = pre ++ [elem] ++ post
  where (pre, _ : post) = splitAt idx ls

get :: Parameter -> [String] -> String
get (Immediate p) _       = show p
get (Pointer   p) program = program !! p

getI :: Parameter -> [String] -> Int
getI p = read . get p

exec :: Instruction -> ([String], Int) -> IO ([String], Int)
exec (Instruction Add [in1, in2, Pointer out]) (program, pointer) = pure
  (replaceAt (show $ a + b) out program, pointer + 4)
 where
  a = getI in1 program
  b = getI in2 program
exec (Instruction Mul [in1, in2, Pointer out]) (program, pointer) = pure
  (replaceAt (show $ a * b) out program, pointer + 4)
 where
  a = getI in1 program
  b = getI in2 program
exec (Instruction Input [Pointer out]) (program, pointer) = do
  input <- getLine
  pure (replaceAt input out program, pointer + 2)
exec (Instruction Output [input]) (program, pointer) = do
  putStrLn $ get input program
  pure (program, pointer + 2)
exec (Instruction JumpT [cond, pos]) (program, pointer)
  | getI cond program == 0 = pure (program, pointer + 3)
  | otherwise              = pure (program, getI pos program)
exec (Instruction JumpF [cond, pos]) (program, pointer)
  | getI cond program /= 0 = pure (program, pointer + 3)
  | otherwise              = pure (program, getI pos program)
exec (Instruction Lt [in1, in2, Pointer out]) (program, pointer) = pure
  (replaceAt (if a < b then "1" else "0") out program, pointer + 4)
 where
  a = getI in1 program
  b = getI in2 program
exec (Instruction Eq [in1, in2, Pointer out]) (program, pointer) = pure
  (replaceAt (if a == b then "1" else "0") out program, pointer + 4)
 where
  a = getI in1 program
  b = getI in2 program
exec inst _ = error $ "invalid instruction: " ++ show inst

getOp :: Instruction -> Op
getOp (Instruction op _) = op

execAll :: [String] -> Int -> IO [String]
execAll program pointer
  | pointer > length program
  = error $ "fallen off end (pointer: " ++ show pointer ++ ")"
  | program !! pointer == "99"
  = pure program
  | otherwise
  = do
    let inst = decode $ drop pointer program
    (program', pointer') <- exec inst (program, pointer)
    execAll program' pointer'

main = do
  program <- map BS.unpack . BS.split ',' <$> BS.readFile "input"
  execAll program 0
