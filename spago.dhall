{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "lists"
  , "integers"
  , "maybe"
  , "node-process"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "transformers"
  , "tuples"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
