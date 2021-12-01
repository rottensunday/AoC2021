module FileAccess where

import Paths_AoC2021
import System.IO
import qualified Data.Text as T

readLines :: FilePath -> IO [T.Text]
readLines path = do
    internalPath <- getDataFileName path
    contents <- readFile internalPath
    return $ T.lines $ T.pack contents