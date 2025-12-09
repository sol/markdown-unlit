module Main where

import Test.Hspec
import Text.Markdown.UnlitSpec

main :: IO ()
main = hspec Text.Markdown.UnlitSpec.spec
