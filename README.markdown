# Literate Haskell support for GitHub's Markdown flavor

> `markdown-unlit` allows you to have a `README.markdown`, that at the same
> time is a *literate Haskell* program.

The following steps show how to set things up, so that:

 * The Haskell code in your README.markdown gets syntax highlighted on GitHub
 * You can run your literate Haskell within GHCi
 * You can create a Cabal `test-suite` from your `README.markdown` (No broken
   code examples anymore. *Yeah!*)

### 1. Install `markdown-unlit`

    $ cabal update && cabal install markdown-unlit


### 2. Create a `README.markdown`


    # nifty-library: Do nifty things (effortlessly!)

    Here is a basic example:

    ~~~ {.haskell .literate}
    main :: IO ()
    main = putStrLn "That was easy!"
    ~~~

    And here is some code that looks nice, but does not yet work:

    ~~~ {.haskell}
    main :: IO ()
    main = launchMissiles
    ~~~

We use fenced code blocks here.  They are supported by GitHub's README
renderer, and many other software packages (e.g. Pandoc).

All code blocks with class `.haskell` are syntax highlighted on GitHub
([like so](https://github.com/sol/markdown-unlit/blob/master/example/README.markdown#readme)).

All code blocks with classes `.haskell` and `.literate` are part of the
literate program.

### 3. Create a symbolic link `README.lhs -> README.markdown`

    $ ln -s README.markdown README.lhs

### 4. Run yor code

At this point we can load the code into GHCi:

    $ ghci -pgmLmarkdown-unlit README.lhs
    *Main> main
    That was easy!

Or better yet, pipe the required flag into a `.ghci` file, and forget about it:

```
$ echo ':set -pgmLmarkdown-unlit' > .ghci
```
```
$ ghci README.lhs
*Main> main
That was easy!
```

### 5. Create a Cabal `test-suite`

```
name:             nifty-library
version:          0.0.0
build-type:       Simple
cabal-version:    >= 1.8

library
  -- nothing here yet

test-suite readme
  type:           exitcode-stdio-1.0
  main-is:        README.lhs
  build-depends:  base
  ghc-options:    -pgmL markdown-unlit
```

Run it like so:

    $ cabal configure --enable-tests && cabal build && cabal test

## Real world examples

 * [attoparsec-parsec](https://github.com/sol/attoparsec-parsec#readme)
 * [hspec-expectations](https://github.com/sol/hspec-expectations#readme)
 * [wai](https://github.com/yesodweb/wai/tree/master/wai#readme)

That's it, have fun!
