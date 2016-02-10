module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Node.FS (FS())
import Node.FS.Sync
import Control.Monad.Eff.Exception (EXCEPTION())
import Swagger.TypeGen

main :: forall e. Eff (console :: CONSOLE, fs :: FS, err :: EXCEPTION | e) Unit
main = do
  spec <- parseYaml <$> readFile "test/fixtures/swagger.yaml"
  let types = generateTypes spec
  log "Hello sailor!"
