module Main where

import           System.Environment
import           Text.Markdown.Unlit

main :: IO ()
main = getArgs >>= run
