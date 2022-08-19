{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "free"
  , "functions"
  , "integers"
  , "maybe"
  , "ordered-collections"
  , "pairs"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "validation"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "test/MyOverrides/**/*.purs" ]
}
