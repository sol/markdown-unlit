{-# LANGUAGE OverloadedStrings #-}
module Text.Markdown.UnlitSpec (main, spec) where

import           Test.Hspec
import           Data.String.Builder

import           Text.Markdown.Unlit

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "parse" $ do
    it "yields an empty list on empty input" $ do
      parse "" `shouldBe` []

    it "parses a code blocks" $ do
      parse . build $ do
        "some text"
        "~~~"
        "some"
        "code"
        "~~~"
        "some other text"
      `shouldBe` [CodeBlock ["some", "code"]]

    it "parses an empty code block" $ do
      parse . build $ do
        "some text"
        "~~~"
        "~~~"
        "some other text"
      `shouldBe` [CodeBlock []]
