{-# LANGUAGE OverloadedStrings #-}
module Movies where

import Control.Applicative ((<|>), empty)
import Data.Char (isSpace)

import qualified Data.Text as T

import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text

import Roman (roman)

type Year = Int
type Month = Int
type Day = Int

data Date = Date Day Month Year

data Suspended = Suspended
               deriving (Show)
data MovieType = Cinema | Video | Television | VideoGame
               deriving (Show)
data Movie = Movie TitleId (Maybe Suspended) MovieType (Maybe Year)
           deriving (Show)

data RunningYears = Finished Year Year | Running Year | Announced
                  deriving (Show, Eq)

data EpisodeTitle = WithTitle T.Text (Maybe (Int, Int))
                  | WithoutTitle (Maybe (Int, Int))
                  | WithoutTitleAndNumbers Year Month Day
                  deriving (Show, Eq)

data TitleId = TitleId T.Text (Maybe Year) (Maybe Int)
              deriving (Show, Eq)

-- {title, season, episode, year_aired}
data Episode = Episode TitleId EpisodeTitle (Maybe Year)
             deriving (Show, Eq)

-- {title, running_years, [season]}
data Series = Series TitleId RunningYears
            deriving (Show, Eq)

-- Temporal stuff

year :: Parser Year
year = do
  yyyy <- count 4 digit
  return $ read yyyy

month :: Parser Month
month = do
  mm <- count 2 digit
  return $ read mm

day :: Parser Day
day = do
  dd <- count 2 digit
  return $ read dd

isoDate :: Char -> Parser Date
isoDate c = do
  yyyy <- decimal
  char c
  mm <- decimal
  char c
  dd <- decimal
  return $ Date dd mm yyyy

finished :: Parser RunningYears
finished = do
  start <- year
  char '-'
  end <- year
  return $ Finished start end

running :: Parser RunningYears
running = do
  start <- year
  char '-'
  string "????"
  return $ Running start

announced :: Parser RunningYears
announced = do
  string "????"
  return Announced

runningYears :: Parser RunningYears
runningYears = finished <|> running <|> announced

aired :: Parser (Maybe Year)
aired = Just <$> year
        <|> string "????" *> return Nothing

-- Parsing logic

-- format: (#1.1)
seasonEpisode :: Parser (Int, Int)
seasonEpisode = (,) <$> (string "(#" *> decimal <* char '.') <*> (decimal <* char ')')

withTitle :: Parser EpisodeTitle
withTitle = do
  t <- manyTill anyChar (lookAhead ((skipSpace >> seasonEpisode >> return ()) <|> (char '}' >> return ())))
  skipSpace
  ep <- option Nothing (Just <$> seasonEpisode)
  return $ WithTitle (T.pack t) ep

withoutTitle :: Parser EpisodeTitle
withoutTitle = do
  ep <- option Nothing (Just <$> seasonEpisode)
  return $ WithoutTitle ep

withoutTitleAndNumbers :: Parser EpisodeTitle
withoutTitleAndNumbers = do
  char '('
  yyyy <- year
  char '-'
  mm <- month
  char '-'
  dd <- day
  char ')'
  return $ WithoutTitleAndNumbers yyyy mm dd

yearId :: Parser (Maybe Year, Maybe Int)
yearId = do
  y <- string "????" *> return Nothing <|> Just <$> year
  n <- (char '/' *> (Just <$> roman)) <|> return Nothing
  return (y, n)

episodeTitle :: Parser EpisodeTitle
episodeTitle = withTitle <|> withoutTitle <|> withoutTitleAndNumbers

episode :: Parser Episode
episode = do
  sid <- seriesId
  skipSpace
  char '{'
  episodeTitle <- episodeTitle
  char '}'
  skipSpace
  susp <- (string "{{SUSPENDED}}" *> return (Just Suspended)) <|> return Nothing
  skipSpace
  yearAired <- aired
  skipTailingWhitespace
  return $ Episode sid episodeTitle yearAired

seriesId :: Parser TitleId
seriesId = do
  char '"'
  t <- manyTill anyChar (string "\" (")
  (y, n) <- yearId
  char ')'
  return $ TitleId (T.pack t) y n

series :: Parser Series
series = do
  sid <- seriesId
  skipSpace
  susp <- (string "{{SUSPENDED}}" *> return (Just Suspended)) <|> return Nothing
  skipSpace
  runYears <- runningYears
  skipTailingWhitespace
  return $ Series sid runYears

movieId :: Parser TitleId
movieId = do
  c <- notChar '"'
  cs <- manyTill anyChar (lookAhead $ char '(' *> yearId <* char ')')
  --skipSpace
  (y, n) <- char '(' *> yearId <* char ')'
  return $ TitleId (T.pack $ c:cs) y n

movieType :: Parser MovieType
movieType = string "(V)" *> return Video
  <|> string "(TV)" *> return Television
  <|> string "(VG)" *> return VideoGame
  <|> return Cinema

movie :: Parser Movie
movie = do
  mId <- movieId
  -- char ')'
  skipSpace
  mType <- movieType
  skipSpace
  susp <- (string "{{SUSPENDED}}" *> return (Just Suspended)) <|> return Nothing
  skipMany (char '\t')
  yearAired <- Just <$> year <|> string "????" *> return Nothing
  skipTailingWhitespace
  return $ Movie mId susp mType yearAired

data MoviesList = S Series | E Episode | M Movie | Error T.Text
                deriving (Show)

moviesListLine :: Parser MoviesList
moviesListLine = S <$> series <|> E <$> episode <|> M <$> movie <|> Error <$> takeTill isEndOfLine

movieParser :: Parser [MoviesList]
movieParser = many' $ moviesListLine <* endOfLine

skipTailingWhitespace :: Parser ()
skipTailingWhitespace = skipMany (satisfy isTailingWhitespace)
  where
    isTailingWhitespace c = c /= '\n' && isSpace c
