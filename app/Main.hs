module Main where

import Lib
import qualified Data.Map as M

main :: IO ()
main = print $ testParser "true /\\ false"