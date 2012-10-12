{-# LANGUAGE OverloadedStrings #-}
module Text.Markdown.UnlitSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Data.String.Builder

import           Text.Markdown.Unlit

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseSelector" $ do
    it "parses + as :&:" $ do
      parseSelector "foo+bar+baz" `shouldBe` Just ("foo" :&: "bar" :&: "baz")

    it "parses whitespace as :|:" $ do
      parseSelector "foo bar baz" `shouldBe` Just ("foo" :|: "bar" :|: "baz")

    it "can handle a combination of :&: and :|:" $ do
      parseSelector "foo+bar baz+bar" `shouldBe` Just ("foo" :&: "bar" :|: "baz" :&: "bar")

    it "is total" $ do
      property $ \xs -> parseSelector xs `seq` True

  describe "unlit" $ do
    it "can be used to unlit everything with a specified class" $ do
      unlit "foo" . build $ do
        "~~~ {.foo}"
        "foo"
        "~~~"
        "~~~ {.bar}"
        "bar"
        "~~~"
      `shouldBe` "foo\n"

    it "can handle :&:" $ do
      unlit ("foo" :&: "bar") . build $ do
        "~~~ {.foo}"
        "some code"
        "~~~"
        "~~~ {.foo .bar}"
        "some other code"
        "~~~"
      `shouldBe` "some other code\n"

    it "can handle :|:" $ do
      unlit ("foo" :|: "bar") . build $ do
        "~~~ {.foo}"
        "foo"
        "~~~"
        "~~~ {.bar}"
        "bar"
        "~~~"
      `shouldBe` "foo\nbar\n"

    it "can handle a combination of :&: and :|:" $ do
      unlit ("foo" :&: "bar" :|: "foo" :&: "baz") . build $ do
        "~~~ {.foo .bar}"
        "one"
        "~~~"
        "~~~ {.foo .baz}"
        "two"
        "~~~"
        "~~~ {.bar .baz}"
        "two"
        "~~~"
      `shouldBe` "one\ntwo\n"

  describe "parse" $ do
    it "yields an empty list on empty input" $ do
      parse "" `shouldBe` []

    it "parses a code block" $ do
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
        "~~~ {.haskell .literate}"
        "some code"
        "~~~"
      `shouldBe` [CodeBlock ["haskell", "literate"] ["some code"]]

  describe "parseClasses" $ do
    it "drops a leading dot" $ do
      parseClasses "~~~ {.foo .bar}" `shouldBe` ["foo", "bar"]

    it "treats dots as whitespace" $ do
      parseClasses "~~~ {foo.bar. ..}" `shouldBe` ["foo", "bar"]
