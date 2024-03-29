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
{ name = "payload"
, dependencies = 
  [ "aff"
  , "affjax"
  , "affjax-node"
  , "argonaut-core"
  , "argonaut-codecs"  
  , "arrays"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "http-methods"
  , "integers"
  , "js-uri"
  , "lists"
  , "maybe"
  , "media-types"
  , "newtype"
  , "node-buffer"
  , "node-http"
  , "node-path"
  , "node-streams"
  , "node-url"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "refs"
  , "strings"
  , "stringutils"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
