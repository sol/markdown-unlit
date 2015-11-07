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
import           System.IO.Temp (withSystemTempFile)
import           System.Directory
import qualified Control.Exception as E

import           Text.Markdown.Unlit


main :: IO ()
main = hspec spec

withTempFile :: (FilePath -> IO ()) -> IO ()
withTempFile action = withSystemTempFile "hspec" $ \f h -> do
  hClose h
  action f `E.finally` removeFile f

spec :: Spec
spec = do
  describe "run" $ do
    it "prints a usage message" $ do
      withProgName "foo" $ do
        (r, Left (ExitFailure 1)) <- hCapture [stderr] (try $ run [])
        r `shouldBe` "usage: foo [selector] -h label infile outfile\n"

    it "unlits code marked with .haskell by default (unless it is marked with .ignore as well)" $ do
      withTempFile $ \infile -> withTempFile $ \outfile -> do
        let withEsc esc = do
              writeFile infile $ unlines
                  [ esc ++ " {.haskell}"
                  , "some code"
                  , esc
                  , esc ++ " {.haskell .ignore}"
                  , "some other code"
                  , esc
                  ]
              run ["-h", "Foo.lhs", infile, outfile]
              readFile outfile `shouldReturn` (build $ do
                "#line 2 \"Foo.lhs\""
                "some code"
                )
        withEsc "~~~"
        withEsc "```"

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

    it "parses ! as Not" $ do
      parseSelector "foo+!bar+baz" `shouldBe` Just ("foo" :&: Not "bar" :&: "baz")

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

    it "can handle Not" $ do
      unlit "Foo.lhs" (Not "foo") . build $ do
        "~~~ {.foo}"
        "1"
        "~~~"
        "~~~ {.bar}"
        "2"
        "~~~"
      `shouldBe` (build $ do
        "#line 5 \"Foo.lhs\""
        "2"
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

    it "can handle a combination of :&: and Not" $ do
      unlit "Foo.lhs" ("foo" :&: Not "bar" :&: "baz") . build $ do
        "~~~ {.foo}"
        "1"
        "~~~"
        "~~~ {.foo .bar}"
        "2"
        "~~~"
        "~~~ {.foo .baz}"
        "3"
        "~~~"
        "~~~ {.foo .bar .baz}"
        "4"
        "~~~"
      `shouldBe` (build $ do
        "#line 8 \"Foo.lhs\""
        "3"
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

    it "parses an indented code block" $ do
      map codeBlockContent . parse . build $ do
        "1. some text"
        "    ~~~"
        "    some"
        "      code"
        "    ~~~"
        "2. some other text"
      `shouldBe` [["some", "  code"]]

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

    it "attaches classes to indented code blocks" $ do
      map codeBlockClasses . parse . build $ do
        "1. some text"
        "    ~~~ {.haskell .literate}"
        "    some code"
        "    ~~~"
        "2. some other text"
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
