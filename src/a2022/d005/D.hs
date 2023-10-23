module D where
  
import Lib
import Data.List as L

type Stacks = [[Char]]

data Cmd = Move {mvQuant :: Int, mvStart :: Int, mvEnd :: Int} deriving(Show)

parseBoxes :: [String] -> Stacks
parseBoxes ls =
  let
    strBoxes = init ls
    parseBoxLine line =
      map fst $ filter (\x -> snd x `rem` 4 == 1) $ zip line [0..]
  in map (filter (/= ' ')) $ transpose $ map parseBoxLine strBoxes
  
parseCommands :: String -> Cmd
parseCommands x = let
  [_,quant,_, start,_, end] = splitOnElem ' ' x
  in Move{mvEnd= readInt end - 1,mvQuant=readInt quant,mvStart=readInt start - 1}

parseInput :: [String] -> (Stacks, [Cmd])
parseInput ls = (parseBoxes a,map parseCommands b)
  where [a, b] = splitOnElem "" ls

runStep :: Stacks -> Cmd -> Stacks
runStep stacks Move{mvQuant=n, mvStart=start, mvEnd=end} = let
 (toMove, stay) = L.splitAt n $ stacks !! start
 newStacks = replaceAt end (toMove++) $ replaceAt start (const stay) stacks
 in newStacks

runCmds :: (Stacks, [Cmd]) -> String
runCmds (stacks, cmds) = let endStacks = L.foldl' runStep stacks cmds
  in map head endStacks
  
execute p = runCmds . parseInput <$> readFileLines p