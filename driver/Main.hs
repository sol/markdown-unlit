{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.IO
import           System.Exit
import           System.Environment

import           Text.Markdown.Unlit

main :: IO ()
main = getArgs >>= \args -> case args of
  -- GHC calls unlit like so:
  --
  -- > unlit -h label Foo.lhs /tmp/somefile
  --
  -- The label is meant to be used in line pragmas, like so:
  --
  -- #line 1 "label"
  --
  ["-h", _, infile, outfile] ->
    fmap (unlit $ "haskell" :&: "literate") (readFile infile) >>= writeFile outfile
  _ -> do
    name <- getProgName
    hPutStrLn stderr ("usage: " ++ name ++ " -h label infile outfile")
    exitFailure
