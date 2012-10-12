{-# LANGUAGE CPP #-}
module Text.Markdown.Unlit (
  unlit
, CodeBlock (..)
, parse
#ifdef TEST
, parseClasses
#endif
) where

import Control.Applicative
import Data.List
import Data.Char

unlit :: String -> String
unlit = unlines . concatMap codeBlockContent . filter (p . codeBlockClasses) . parse
  where
    p = (&&) <$> elem "literate" <*> elem "haskell"

data CodeBlock = CodeBlock {
  codeBlockClasses :: [String]
, codeBlockContent :: [String]
} deriving (Eq, Show)

parse :: String -> [CodeBlock]
parse = go . lines
  where
    go :: [String] -> [CodeBlock]
    go xs = case break isFence xs of
      (_, [])   -> []
      (_, y:ys) -> case takeCB y ys of
        (cb, rest) -> cb : go rest

    takeCB :: String -> [String] -> (CodeBlock, [String])
    takeCB fence xs = case break isFence xs of
      (cb, rest) -> (CodeBlock (parseClasses fence) cb, drop 1 rest)

parseClasses :: String -> [String]
parseClasses xs = case dropWhile isSpace . dropWhile (== '~') $ xs of
  '{':ys -> words . replace '.' ' ' . takeWhile (/= '}') $ ys
  _      -> []

isFence :: String -> Bool
isFence = isPrefixOf "~~~"

replace :: Char -> Char -> String -> String
replace x sub = map f
  where
    f y | x == y    = sub
        | otherwise = y
