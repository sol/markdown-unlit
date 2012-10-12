module Text.Markdown.Unlit where

import Data.List

data CodeBlock = CodeBlock [String]
  deriving (Eq, Show)

parse :: String -> [CodeBlock]
parse = go . lines
  where
    go :: [String] -> [CodeBlock]
    go xs = case break isFence xs of
      (_, [])   -> []
      (_, _:ys) -> case takeCB ys of
        (cb, rest) -> cb : go rest

    takeCB :: [String] -> (CodeBlock, [String])
    takeCB xs = case break isFence xs of
      (cb, rest) -> (CodeBlock cb, drop 1 rest)

    isFence :: String -> Bool
    isFence = isPrefixOf "~~~"
