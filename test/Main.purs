module Test.Main where

import Prelude
import Data.Tuple
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Node.FS (FS())
import Node.FS.Sync
import Control.Monad.Eff.Exception (EXCEPTION())
import Swagger.TypeGen
import Data.Either.Unsafe
import Debug.Trace
import Data.Foldable (for_)

main :: forall e. Eff (console :: CONSOLE, fs :: FS, err :: EXCEPTION | e) Unit
main = do
  spec <- parseYaml <$> readFile "test/fixtures/swagger.yaml"

  let types = fromRight $ generateTypes spec
  for_ types \(Tuple req res) -> do
    log $ renderType req

  log "Hello sailor!"
