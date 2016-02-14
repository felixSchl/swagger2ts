module Test.Main where

import Prelude

import Control.Monad.Aff
import Test.Spec                  (describe, pending, it)
import Test.Spec.Runner           (run)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple

import Swagger.TypeGen
import Node.FS.Sync (readFile)
import Data.Either.Unsafe (fromRight)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Foldable (for_)

main = run [consoleReporter] do
  describe "swagger2ts" do
    describe "request/response generation" do
      it "works" do
        spec <- parseYaml <$> do
          liftEff $ readFile "test/fixtures/swagger.yaml"
        let types = fromRight $ generateTypes spec
        let expected = SwaggerTypes {
              server: [
                ReqRes {
                  request:  InterfaceType "CheckHealthRequest" [] []
                , response: StringType
                }
              ]
            , client: [
                ReqRes {
                  request:  InterfaceType "CheckHealthRequest" [] []
                , response: StringType
                }
              ]
            }
        liftEff $ assertEqual expected types
