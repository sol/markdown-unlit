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
      map codeBlockContent . parse . build $ do
        "some text"
        "~~~"
        "some"
        "code"
        "~~~"
        "some other text"
      `shouldBe` [["some", "code"]]

    it "parses an empty code block" $ do
      map codeBlockContent . parse . build $ do
        "some text"
        "~~~"
        "~~~"
        "some other text"
      `shouldBe` [[]]

    it "attaches classes to code blocks" $ do
      parse . build $ do
        "~~~ {haskell literate}"
        "some code"
        "~~~"
      `shouldBe` [CodeBlock ["haskell", "literate"] ["some code"]]
