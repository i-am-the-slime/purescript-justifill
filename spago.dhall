{ name = "justifill"
, dependencies =
  [ "aff", "effect", "maybe", "prelude", "record", "spec", "typelevel-prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, repository = "https://github.com/i-am-the-slime/purescript-justifill.git"
, license = "Apache-2.0"
}
