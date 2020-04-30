{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "react-queue"
, dependencies =
  [ "exceptions", "queue", "react", "zeta" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
