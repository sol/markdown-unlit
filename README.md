# Literate Haskell support for Markdown

`markdown-unlit` is a custom `unlit` program.  It can be used to extract
Haskell code from Markdown files.

To use it with GHC, add

    ghc-options: -pgmL markdown-unlit

to your cabal file.

## Extended example

> tl;dr `markdown-unlit` allows you to have a `README.md`, that at the
> same time is a literate Haskell program.

The following steps show you how to set things up, so that:

 * the Haskell code in your `README.md` gets syntax highlighted on GitHub
 * you can run your literate Haskell within GHCi
 * you can create a Cabal `test-suite` from your `README.md` (no broken code
   examples anymore, yay!)

The complete code of this example is provided in the [`example`](https://github.com/sol/markdown-unlit/tree/master/example) subdirectory.

### 1. Install `markdown-unlit`

    $ cabal update && cabal install markdown-unlit


### 2. Create a `README.md`


    # nifty-library: Do nifty things (effortlessly!)

    Here is a basic example:

    ```haskell
    main :: IO ()
    main = putStrLn "That was easy!"
    ```

Code blocks with class `haskell` are syntax highlighted on GitHub ([like
so](https://github.com/sol/markdown-unlit/blob/master/example/README.md#readme)).

### 3. Create a symbolic link `README.lhs -> README.md`

    $ ln -s README.md README.lhs

### 4. Run your code

At this point we can load the code into GHCi:

    $ ghci -pgmL markdown-unlit README.lhs
    *Main> main
    That was easy!

Or better yet, pipe the required flag into a `.ghci` file, and forget about it:

```
$ echo ':set -pgmL markdown-unlit' > .ghci
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
  build-depends:  base, markdown-unlit
  ghc-options:    -pgmL markdown-unlit
```

Run it like so:

    $ cabal configure --enable-tests && cabal build && cabal test

## Customizing

By default, `markdown-unlit` extracts all code that is marked with `haskell`,
unless it is marked with `ignore` as well.  You can customize this by passing
`-optL <pattern>` to GHC.

A simple pattern is just a class name, e.g.:

    -optL foo

extracts all code that is marked with `foo`.

A class name can be negated by prepending it with a `!`, e.g.

    -optL !foo

extracts all code, unless it is marked with `foo`.

You can use `+` to combine two patterns with *AND*, e.g.

    -optL foo+bar

extracts all code that is marked with both `foo` and `bar`.

If `-optL` is given multiple times, the patterns are combined with *OR*, e.g.

    -optL foo -optL bar

extracts all code that is either marked with `foo` or `bar`.

## Development

### Limitations

 * [indented code blocks](http://daringfireball.net/projects/markdown/syntax#precode) are not yet supported

If you want to get any limitation lifted, open a ticket or send a pull request.

### Contributing

Add tests for new code, and make sure that the test suite passes with your
modifications.

    cabal configure --enable-tests && cabal build && cabal test

## Real world examples

 * [attoparsec-parsec](https://github.com/sol/attoparsec-parsec#readme)
 * [hspec-expectations](https://github.com/sol/hspec-expectations#readme)
 * [wai](https://github.com/yesodweb/wai/tree/master/wai#readme)

That's it, have fun!
