{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "justifill"
, dependencies =
    [ "record", "typelevel-prelude", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
