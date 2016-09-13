{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either (rights)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (stderr, stdout, hPutStrLn)

import qualified Codec.Compression.GZip as GZip
import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.IO as IO (getContents, readFile, hPutStrLn)
import qualified Data.Text as T (Text, all, intercalate, lines, pack)

import Movies (MoviesList (..), Movie (..), Series (..), Episode (..), TitleId (..), moviesListLine, movieParser)

main :: IO ()
main = do
  ms <- parseMovies' ""
  mapM_ handleMovies (zip [1..] ms)

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
  --contents <- IO.readFile moviesPath
  contents <- IO.getContents
  --let moviesList = dropWhile (T.all (== '#')) $ T.lines contents
  return $ rights $ map (parseOnly moviesListLine) (T.lines contents)
  --return $ rights $ map (parseOnly moviesListLine) (T.lines contents)

handleMovies :: (Int, MoviesList) -> IO ()
handleMovies (_, Error t) = IO.hPutStrLn stderr t
handleMovies (i, m) = IO.hPutStrLn stdout $ movieToString m

movieToString :: MoviesList -> T.Text
movieToString (M (Movies.Movie tid _ _ _)) = titleIdToString tid
movieToString (S (Movies.Series tid _)) = titleIdToString tid
movieToString (E (Movies.Episode tid _ _)) = titleIdToString tid
movieToString _ = ""

titleIdToString :: Movies.TitleId -> T.Text
titleIdToString (Movies.TitleId t (Just y) (Just n)) =
  T.intercalate "\t" [t, T.pack $ show y, T.pack $ show n]
titleIdToString (Movies.TitleId t (Just y) Nothing) =
  T.intercalate "\t" [t, T.pack $ show y, "\"\""]
titleIdToString (Movies.TitleId t Nothing (Just n)) =
  T.intercalate "\t" [t, "\"\"", T.pack $ show n]
titleIdToString (Movies.TitleId t Nothing Nothing) =
  T.intercalate "\t" [t, "\"\"", "\"\""]

