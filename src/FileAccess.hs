module FileAccess where

import Paths_AoC2021
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readLines :: FilePath -> IO [T.Text]
readLines path = do
  internalPath <- getDataFileName path
  contents <- TIO.readFile internalPath
  return $ T.lines contents

readWholeFile :: FilePath -> IO T.Text
readWholeFile path = do
  internalPath <- getDataFileName path
  TIO.readFile internalPath