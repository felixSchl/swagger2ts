module Test.Main where

import Prelude

import           Swagger.TypeGen
import qualified Swagger.Tasks as Tasks
import           Control.Monad.Aff
import           Test.Spec (describe, pending, it)
import           Test.Spec.Runner (run)
import           Test.Spec.Assertions (shouldEqual)
import           Test.Spec.Reporter.Console (consoleReporter)
import           Test.Assert.Simple
import           Data.Maybe (Maybe(..))
import           Node.FS.Sync (readFile)
import           Data.Either.Unsafe (fromRight)
import           Control.Monad.Eff.Class (liftEff)
import           Control.Monad.Eff.Console (log)
import           Data.Foldable (for_)
import           Debug.Trace
import qualified Test.Fixtures.Petstore as Petstore

main = run [consoleReporter] do

  describe "swagger2ts" do
    describe "request/response generation" do
      it "should understand the stock petstore yaml" do
        spec <- parseYaml <$> do
          liftEff $ readFile "test/fixtures/petstore.yaml"
        let types = fromRight $ generateTypes spec
            expected = Petstore.expected
        liftEff $ assertEqual types expected

    describe "task" do
      it "should emit typings to disk" do
        Tasks.generateTypes "" "./fixtures/generated/"
