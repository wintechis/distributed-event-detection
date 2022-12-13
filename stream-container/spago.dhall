{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "stream-container"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "formatters"
  , "httpure"
  , "integers"
  , "lists"
  , "maybe"
  , "n3"
  , "now"
  , "numbers"
  , "optparse"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "rdf"
  , "refs"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
