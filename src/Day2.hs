{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day2 where

import FileAccess
import qualified Data.Text as T
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor


type Parser = Parsec Void T.Text
data Command
  = Forward Int
  | Down Int
  | Up Int deriving (Show)

inputs = fmap processLine <$> readLines "resources/Day2Input.txt"


commandParser :: Parser Command
commandParser
  =   string "forward " *> fmap Forward L.decimal
  <|> string "down " *> fmap Down L.decimal
  <|> string "up " *> fmap Up L.decimal

processLine :: T.Text -> Command
processLine t = either (\x -> Down 0) id (parse commandParser "" t)

day2Task1 = fmap (uncurry (*) . foldr processCommand (0,0)) inputs
  where processCommand :: Command -> (Int, Int) -> (Int, Int)
        processCommand (Forward x) (x', y) = (x + x', y)
        processCommand (Down y) (x, y') = (x, y + y')
        processCommand (Up y) (x, y') = (x, y' - y)

day2Task2 = fmap ((\(x,y,z) -> x * y) . foldl processCommand (0,0,0)) inputs
  where processCommand :: (Int, Int, Int) -> Command  -> (Int, Int, Int)
        processCommand (x', y, a) (Forward x) = (x + x', y + (a * x), a)
        processCommand (x, y', a) (Down y) = (x, y', y + a)
        processCommand (x, y', a) (Up y) = (x, y', a - y)