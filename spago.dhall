{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "exceptions"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
