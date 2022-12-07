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
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-node"
  , "arrays"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "formatters"
  , "http-methods"
  , "integers"
  , "js-timers"
  , "lists"
  , "maybe"
  , "n3"
  , "now"
  , "numbers"
  , "optparse"
  , "ordered-collections"
  , "parallel"
  , "parsing"
  , "prelude"
  , "rdf"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
