{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Text.Markdown.Unlit (
  run
, unlit
, Selector (..)
, parseSelector
, CodeBlock (..)
, parse
#ifdef TEST
, parseReorderingKey
, parseClasses
#endif
) where

import           Prelude ()
import           Prelude.Compat
import           Control.Arrow
import           Data.Char
import           Data.List.Compat
import           Data.Maybe
import           Data.String
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Read

fenceChars :: [Char]
fenceChars = ['`', '~']

fences :: [String]
fences = map (replicate 3) fenceChars

-- | Program entry point.
run :: [String] -> IO ()
run args =
  -- GHC calls unlit like so:
  --
  -- > unlit [args] -h label Foo.lhs /tmp/somefile
  --
  -- [args] are custom arguments provided with -optL
  --
  -- The label is meant to be used in line pragmas, like so:
  --
  -- #line 1 "label"
  --
  case break (== "-h") args of
    (mkSelector -> selector, "-h" : files) -> case files of
      [src, cur, dst] -> do
        readFileUtf8 cur >>= writeFileUtf8 dst . unlit src selector
      [src] -> do
        readFileUtf8 src >>= writeUtf8 stdout . unlit src selector
      _ -> usage
    _ -> usage
    where
      usage :: IO ()
      usage = do
        name <- getProgName
        hPutStrLn stderr ("usage: " ++ name ++ " [selector] -h SRC CUR DST")
        exitFailure

      mkSelector :: [String] -> Selector
      mkSelector = fromMaybe ("haskell" :&: Not "ignore") . parseSelector . unwords

      readFileUtf8 :: FilePath -> IO String
      readFileUtf8 name = openFile name ReadMode >>= \ handle -> hSetEncoding handle utf8 >> hGetContents handle

      writeFileUtf8 :: FilePath -> String -> IO ()
      writeFileUtf8 name str = withFile name WriteMode $ \ handle -> writeUtf8 handle str

      writeUtf8 :: Handle -> String -> IO ()
      writeUtf8 handle str = hSetEncoding handle utf8 >> hPutStr handle str

unlit :: FilePath -> Selector -> String -> String
unlit src selector = unlines . concatMap formatCodeBlock . sortCodeBlocks . filter (toPredicate selector . codeBlockClasses) . parse
  where
    formatCodeBlock :: CodeBlock -> [String]
    formatCodeBlock cb = ("#line " ++ show (codeBlockStartLine cb) ++ " " ++ show src) : codeBlockContent cb

    sortCodeBlocks :: [CodeBlock] -> [CodeBlock]
    sortCodeBlocks = map fst . sortOn snd . addSortKey
      where
        addSortKey :: [CodeBlock] -> [(CodeBlock, (ReorderingKey, DeclarationOrder))]
        addSortKey = zipWith ((id &&&) . sortKey) [0..]

        sortKey :: a -> CodeBlock -> (ReorderingKey, a)
        sortKey n code = (reorderingKey code, n)

    toPredicate :: Selector -> [String] -> Bool
    toPredicate = go
      where
        go s = case s of
          Class c -> elem c
          Not p   -> not . go p
          a :&: b -> (&&) <$> go a <*> go b
          a :|: b -> (||) <$> go a <*> go b

newtype DeclarationOrder = DeclarationOrder Int
  deriving newtype (Eq, Ord, Enum, Num)

newtype ReorderingKey = ReorderingKey Int
  deriving newtype (Eq, Show, Read, Ord, Bounded, Num)

reorderingKey :: CodeBlock -> ReorderingKey
reorderingKey = parseReorderingKey . codeBlockClasses

parseReorderingKey :: [String] -> ReorderingKey
parseReorderingKey = go
  where
    go :: [String] -> ReorderingKey
    go = \ case
      [] -> 0
      "top" : _ -> minBound
      ('t' : 'o' : 'p' : ':' : (readMaybe -> Just n)) : _ -> minBound + n
      _ : classes -> go classes

infixr 3 :&:
infixr 2 :|:

data Selector
  = Class String
  | Not Selector
  | Selector :&: Selector
  | Selector :|: Selector
  deriving (Eq, Show)

parseSelector :: String -> Maybe Selector
parseSelector input = case words input of
  [] -> Nothing
  xs -> (Just . foldr1 (:|:) . map parseAnds) xs
  where
    parseAnds = foldr1 (:&:) . map parseClass . split (== '+')

    parseClass c = case c of
      '!':xs -> Not (Class xs)
      _      -> Class c

    -- a copy from https://github.com/sol/string
    split :: (Char -> Bool) -> String -> [String]
    split p = go
      where
        go xs = case break p xs of
          (ys, [])   -> [ys]
          (ys, _:zs) -> ys : go zs

instance IsString Selector where
  fromString = Class

data CodeBlock = CodeBlock {
  codeBlockClasses   :: [String]
, codeBlockContent   :: [String]
, codeBlockStartLine :: Int
} deriving (Eq, Show)

type Line = (Int, String)

parse :: String -> [CodeBlock]
parse = go . zip [2..] . lines
  where
    go :: [Line] -> [CodeBlock]
    go xs = case break isFence xs of
      (_, [])   -> []
      (_, y:ys) -> case takeCB y ys of
        (cb, rest) -> cb : go rest

    takeCB :: Line -> [Line] -> (CodeBlock, [Line])
    takeCB (n, fence) xs =
      let indent = length . takeWhile isSpace $ fence
      in case break isFence xs of
        (cb, rest) -> (CodeBlock (parseClasses fence) (map (drop indent . snd) cb) n, drop 1 rest)

    isFence :: Line -> Bool
    isFence = p . dropWhile isSpace . snd
      where
        p :: String -> Bool
        p line = any (`isPrefixOf` line) fences

parseClasses :: String -> [String]
parseClasses xs = words . replace '.' ' ' $ case dropWhile isSpace . dropWhile (`elem` fenceChars) . dropWhile isSpace $ xs of
  '{':ys -> takeWhile (/= '}') ys
  ys -> ys

replace :: Char -> Char -> String -> String
replace x sub = map f
  where
    f y | x == y    = sub
        | otherwise = y
