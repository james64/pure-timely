{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "maybe"
  , "node-process"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "tuples"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
