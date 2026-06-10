module Main where

import Test.Hspec
import qualified Text.Markdown.UnlitSpec

main :: IO ()
main = hspec Text.Markdown.UnlitSpec.spec
