{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either (rights)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (stderr, stdout, hPutStrLn)

import qualified Codec.Compression.GZip as GZip
import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.IO as IO (readFile, hPutStrLn)
import qualified Data.Text as T (Text, all, lines, pack)

import Movies (MoviesList (..), moviesListLine, movieParser)

main :: IO ()
main = do
  args <- getArgs
  let moviesPath = args !! 0
  exists <- doesFileExist moviesPath
  if exists
    then do
      ms <- parseMovies' moviesPath
      mapM_ handleMovies (zip [1..] ms)
    else hPutStrLn stderr $ moviesPath ++ " does not exist!"

parseCompressed :: FilePath -> IO B.ByteString
parseCompressed path = do
  contents <- GZip.decompress <$> B.readFile path
  return contents

test :: FilePath -> IO ()
test moviesPath = do
  exists <- doesFileExist moviesPath
  if exists
    then do
      ms <- parseMovies' moviesPath
      mapM_ handleMovies (zip [1..] ms)
    else hPutStrLn stderr $ moviesPath ++ " does not exist!"

-- DANGEROUS: needs to consume all the input; no pipelining
parseMovies :: FilePath -> IO [MoviesList]
parseMovies moviesPath = do
  contents <- IO.readFile moviesPath
  let res = parseOnly movieParser contents
  case res of
    (Left msg) -> IO.hPutStrLn stderr (T.pack msg) >> return []
    (Right movie) -> return movie

parseMovies' :: FilePath -> IO [MoviesList]
parseMovies' moviesPath = do
  contents <- IO.readFile moviesPath
  --let moviesList = dropWhile (T.all (== '#')) $ T.lines contents
  return $ rights $ map (parseOnly moviesListLine) (T.lines contents)
  --return $ rights $ map (parseOnly moviesListLine) (T.lines contents)

handleMovies :: (Int, MoviesList) -> IO ()
handleMovies (_, Error t) = IO.hPutStrLn stderr t
handleMovies m = hPutStrLn stdout $ show m

