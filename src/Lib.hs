module Lib
    ( someFunc
    ) where

import Types
import Parser (testParser, true)

someFunc :: IO ()
someFunc = print $ testParser true [TRUE]
