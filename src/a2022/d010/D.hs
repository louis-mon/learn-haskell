module D where

import Lib
import Data.List.Split as Split
import qualified Data.List as L

data Cmd = Noop | Addx Int deriving (Show)

type Program = [Cmd]

data State = State Int deriving (Show)

parseCmd :: String -> Cmd
parseCmd str =
  let (cmd:rest) = splitOnElem ' ' str
  in case cmd of
    "noop" -> Noop
    "addx" -> Addx $ readInt $ head rest

updateState :: State -> Cmd -> [State]
updateState (State reg) cmd = case cmd of
  Noop -> [State reg]
  Addx value -> [State (reg + value), State reg]

executeCmds :: [Cmd] -> [State]
executeCmds = reverse . L.foldl' (\acc@(h:_) cmd -> updateState h cmd++acc) [State 1]

computeSignal :: [State] -> Int
computeSignal = sum . map (\(i,State reg) -> i * reg) . filter (\(i,_) -> (i - 20) `mod` 40 == 0) . zip [1..]

drawPixel :: Int -> State -> Char
drawPixel tick (State reg) = if abs (tick - reg) <= 1 then '#' else '.'

drawImage :: [State] -> String
drawImage = unlines . map (zipWith drawPixel [0..]) . chunksOf 40

sol1 = computeSignal . executeCmds . map parseCmd
sol2 = drawImage . executeCmds . map parseCmd

test = runSample "10" sol2

execute = runInput "10" sol2