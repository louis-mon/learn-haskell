module Lib
  ( readInt,
    splitOnElem,
    readFileLines,
    replaceAt,
    map2d,
    zipWith2d,
    takeWhile2,
    runInput,
    runSample,
    testSample,
    findIndex2d,
    readSample,
    readInput,
    parseOrError,
    padZeros,
    Parser,
    sourceColI
  )
where
  
import Data.List as L
import Data.Function
import Data.Maybe
import Text.Printf
import qualified Data.List.Split as Split

import Text.Megaparsec hiding(State)
import Data.Void

type Parser = Parsec Void String

sourceColI :: SourcePos -> Int
sourceColI = (subtract 1) . unPos . sourceColumn

readInt :: String -> Int
readInt = read

splitOnElem :: (Eq a) => a -> [a] -> [[a]]
splitOnElem e l =
  let g = L.groupBy ((==) `on` (== e)) l
  in map fst $ filter (even . snd) $ zip g [0..]

takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
takeWhile2 f l = let
  z = zip (init l) (tail l)
  matched = L.takeWhile (uncurry f) z
  in map fst matched
  
readFileLines :: String -> IO [String]
readFileLines x = lines <$> readFile x

replaceAt :: Int -> (a -> a) -> [a] -> [a]
replaceAt idx f l = let
  (a, b1:rb) = splitAt idx l
  in a ++ f b1:rb
  
map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

zipWith2d :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith2d f = zipWith (zipWith f)

findIndex2d :: (a -> Bool) -> [[a]] -> Maybe (Int, Int)
findIndex2d f = listToMaybe . catMaybes . zipWith (fmap . flip (,)) [0..] . map (findIndex f)

padZeros :: Int -> String
padZeros = printf "%03d"

year :: String
year = "a2023"

sampleFile :: Int -> FilePath
sampleFile i = "src/" ++ year ++ "/d" ++ padZeros i ++ "/sample.txt"

inputFile :: Int -> FilePath
inputFile i = "src/" ++ year ++ "/d" ++ padZeros i ++ "/input.txt"

testSample :: (Eq a) => Int -> a -> ([String] -> a) -> IO (Bool, a)
testSample i a f = do
  r <- f <$> readFileLines (sampleFile i)
  return (r == a, r)

runSample :: Int -> ([String] -> a) -> IO a
runSample i f = f <$> readFileLines (sampleFile i)
  
runInput :: Int -> ([String] -> a) -> IO a
runInput i f = f <$> readFileLines (inputFile i)

parseOrError :: Parser a -> String -> a
parseOrError p input = either (error . errorBundlePretty) id $ parse p "f" input

readSample :: Int -> IO String
readSample i = readFile (sampleFile i)

readInput :: Int -> IO String
readInput i = readFile (inputFile i)