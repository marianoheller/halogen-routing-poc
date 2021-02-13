{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-bus"
  , "affjax"
  , "argonaut"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "formatters"
  , "halogen"
  , "now"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
