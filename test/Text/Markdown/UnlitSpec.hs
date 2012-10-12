{-# LANGUAGE OverloadedStrings #-}
module Text.Markdown.UnlitSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Data.String.Builder
import           System.Environment
import           Control.Exception
import           System.Exit
import           System.IO.Silently
import           System.IO
import           System.Directory
import qualified Control.Exception as E

import           Text.Markdown.Unlit

main :: IO ()
main = hspec spec

withTempFile :: (FilePath -> IO ()) -> IO ()
withTempFile action = do
  (f, h) <- openTempFile "." "hspec-tmp"
  hClose h
  action f `E.finally` removeFile f

spec :: Spec
spec = do
  describe "run" $ do
    it "prints a usage message" $ do
      withProgName "foo" $ do
        (r, Left (ExitFailure 1)) <- hCapture [stderr] (try $ run [])
        r `shouldBe` "usage: foo [selector] -h label infile outfile\n"

    it "unlits code marked with .haskell by default" $ do
      withTempFile $ \infile -> withTempFile $ \outfile -> do
        writeFile infile . build $ do
          "~~~ {.haskell}"
          "some code"

          "~~~"
          "~~~ {.not-haskell}"
          "some other code"

          "~~~"
        run ["-h", "Foo.lhs", infile, outfile]
        readFile outfile `shouldReturn` (build $ do
          "#line 2 \"Foo.lhs\""
          "some code"
          )

    it "can be customized" $ do
      withTempFile $ \infile -> withTempFile $ \outfile -> do
        writeFile infile . build $ do
          "~~~ {.foo}"
          "some code"
          ""
          "~~~"
          "~~~ {.bar}"
          "some other code"
          "~~~"
        run ["bar", "-h", "Foo.lhs", infile, outfile]
        readFile outfile `shouldReturn` (build $ do
          "#line 6 \"Foo.lhs\""
          "some other code"
          )

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
      unlit "Foo.lhs" "foo" . build $ do
        "~~~ {.foo}"
        "foo"
        "~~~"
        "~~~ {.bar}"
        "bar"
        "~~~"
      `shouldBe` (build $ do
        "#line 2 \"Foo.lhs\""
        "foo"
        )

    it "can handle :&:" $ do
      unlit "Foo.lhs" ("foo" :&: "bar") . build $ do
        "~~~ {.foo}"
        "some code"
        "~~~"
        "~~~ {.foo .bar}"
        "some other code"
        "~~~"
      `shouldBe` (build $ do
        "#line 5 \"Foo.lhs\""
        "some other code"
        )

    it "can handle :|:" $ do
      unlit "Foo.lhs" ("foo" :|: "bar") . build $ do
        "~~~ {.foo}"
        "foo"
        "~~~"
        "~~~ {.bar}"
        "bar"
        "~~~"
      `shouldBe` (build $ do
        "#line 2 \"Foo.lhs\""
        "foo"
        "#line 5 \"Foo.lhs\""
        "bar"
        )

    it "can handle a combination of :&: and :|:" $ do
      unlit "Foo.lhs" ("foo" :&: "bar" :|: "foo" :&: "baz") . build $ do
        "~~~ {.foo .bar}"
        "one"
        "~~~"
        "~~~ {.foo .baz}"
        "two"
        "~~~"
        "~~~ {.bar .baz}"
        "two"
        "~~~"
      `shouldBe` (build $ do
        "#line 2 \"Foo.lhs\""
        "one"
        "#line 5 \"Foo.lhs\""
        "two"
        )

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
      map codeBlockClasses . parse . build $ do
        "~~~ {.haskell .literate}"
        "some code"
        "~~~"
      `shouldBe` [["haskell", "literate"]]

    it "attaches source locations to code blocks" $ do
      map codeBlockStartLine . parse . build $ do
        "some text"
        ""
        "~~~"
        "some"
        "code"
        "~~~"
        "some other text"
      `shouldBe` [4]

  describe "parseClasses" $ do
    it "drops a leading dot" $ do
      parseClasses "~~~ {.foo .bar}" `shouldBe` ["foo", "bar"]

    it "treats dots as whitespace" $ do
      parseClasses "~~~ {foo.bar. ..}" `shouldBe` ["foo", "bar"]
