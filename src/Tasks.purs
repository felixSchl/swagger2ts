module Swagger.Tasks where

import Prelude
import Control.Monad.Aff
import Node.FS (FS())
import Node.FS.Aff (rmdir)
import Control.Monad.Error.Class (throwError, catchError)
import Data.Either
import qualified Control.Monad.Eff.Exception as Err

generateTypes :: forall e. String -> String -> Aff (fs :: FS | e) Unit
generateTypes spec outdir = do
  result <- attempt (rmdir outdir)
  case result of
    -- XXX: Wont' work, need the error 'code':
    Left e | Err.message e /= "ENOENT" -> throwError e
    _                                  -> pure unit
