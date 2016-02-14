module Test.Main where

import Prelude

import Control.Monad.Aff
import Test.Spec                  (describe, pending, it)
import Test.Spec.Runner           (run)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple

import Swagger.TypeGen
import Data.Maybe (Maybe(..))
import Node.FS.Sync (readFile)
import Data.Either.Unsafe (fromRight)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Foldable (for_)

main = run [consoleReporter] do
  describe "swagger2ts" do
    describe "request/response generation" do
      it "should understand the stock petstore yaml" do
        spec <- parseYaml <$> do
          liftEff $ readFile "test/fixtures/petstore.yaml"
        let types = fromRight $ generateTypes spec
            expected = SwaggerTypes {
              client: [
                ReqRes {
                  request: InterfaceType "ListPetsRequest" [] [
                    Field {
                      default:  Nothing
                    , name:     "limit"
                    , required: false
                    , type:     NumberType
                    }
                  ]
                , response: StringType
                }
              , ReqRes {
                  request: InterfaceType "CreatePetsRequest" [] []
                , response: StringType
                }
              , ReqRes {
                  request: InterfaceType "ShowPetByIdRequest" [] [
                    Field {
                      default:  Nothing
                    , name:     "petId"
                    , required: true
                    , type:     StringType
                    }
                  ]
                , response: StringType
                }
              ]
            , server: [
                ReqRes {
                  request: InterfaceType "ListPetsRequest" [] [
                    Field {
                      default:  Nothing
                    , name:     "query"
                    , required: false
                    , type:     ObjectType [
                        Field {
                          default:  Nothing
                        , name:     "limit"
                        , required: false
                        , type:     NumberType
                        }
                      ]
                    }
                  ]
                , response: StringType
                }
              , ReqRes {
                  request: InterfaceType "CreatePetsRequest" [] []
                , response: StringType
                }
              , ReqRes {
                  request: InterfaceType "ShowPetByIdRequest" [] [
                    Field {
                      default:  Nothing
                    , name:     "path"
                    , required: false
                    , type:     ObjectType [
                        Field {
                          default:  Nothing
                        , name:     "petId"
                        , required: true
                        , type:     StringType
                        }
                      ]
                    }
                  ]
                , response: StringType
                }
              ]
            }

        liftEff $ assertEqual types expected
