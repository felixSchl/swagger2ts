module Swagger.TypeGen where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Foreign
import Data.Foreign.Index
import Data.Foreign.Keys
import Data.Foreign.Class
import Debug.Trace
import Node.Buffer hiding (read, readString)
import Data.Generic
import Control.Bind ((=<<))
import Data.Array (foldM, (:))
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Foldable (any)
import Data.String (replace)

foreign import parseYaml :: Buffer -> Foreign

newtype Field = Field {
  name     :: String
, required :: Boolean
, type     :: Type
, default  :: Maybe Value
}

field :: String -> Boolean -> Type -> Maybe Value -> Field
field n r t d = Field { name: n, required: r, type: t, default: d }

data Value
  = StringValue
  | BooleanValue
  | NumberValue
  | ObjectValue
  | ArrayValue

type Name = String
type Extends = Array String
type Implements = Array String
type Types = Array Type
type Fields = Array Field

data Type
  = StringType
  | BooleanType
  | NumberType
  | ObjectType Fields
  | ArrayType Type
  | ReferenceType Name
  | UnitType
  | ClassType Name
  | InterfaceType Name Extends Fields
  | AnonUnionType
  | UnionType Name Types

derive instance genericType :: Generic Type
instance showType :: Show Type where
  show = gShow

derive instance genericValue :: Generic Value
instance showValue :: Show Value where
  show = gShow

derive instance genericField :: Generic Field
instance showField :: Show Field where
  show = gShow

-- Convert a swagger schema into a type.
toType :: Foreign -> F Type
toType schema = fromRefField <|> fromTypeField
  where
    fromRefField = ReferenceType <$> do
      ref <- readString =<< readProp "$ref" schema
      return $
        replace "/" "." (
          replace "#/" "" ref
        )

    fromTypeField = do
      typ <- readString =<< readProp "type" schema
      case typ of
        "string"  -> pure StringType
        "number"  -> pure NumberType
        "boolean" -> pure BooleanType
        "array"   -> toArray
        "object"  -> toObject
        _         -> Left $ JSONError $ "Unknown type: `" ++ typ ++ "`"
      where
        toObject = do
          props <- readProp "properties" schema
          ObjectType <$> do
            keys props >>= foldM (step props) []
          where
            step :: Foreign -> Array Field -> String -> F (Array Field)
            step props acc name = do
              typ <- toType =<< readProp name props
              requiredFields <-
                (traverse readString =<< readArray
                                     =<< readProp "required" schema)
                <|> return []
              return $
                Field {
                  name:     name
                , required: any (== name) requiredFields
                , type:     typ
                , default:  Nothing
                } : acc
        toArray = ArrayType <$> do
          toType =<< readProp "items" schema

-- Derive type information from a swagger spec.
generateTypes :: Foreign -> F Unit
generateTypes spec = do
  definitions <- genDefTypes "definitions"
  parameters  <- genDefTypes "parameters"
  responses   <- genDefTypes "responses"
  operations  <- genOpTypes
  traceShowA operations
  return unit

  where
    genOpTypes :: F Unit
    genOpTypes = do
      paths <- readProp "paths" spec
      keys paths >>= traverse (genOpType paths)
      return unit

      where
        genOpType :: Foreign -> String -> F Unit
        genOpType def path = do
          -- TODO: Generate the request/response types for server/client modules
          return unit
            where

              -- | Generate the server request type.
              -- | This type is compatible with the object produced by
              -- | the `swagger-express-middleware` module, and can be used
              -- | as follows:
              -- |
              -- | ```
              -- | function(request: <GeneratedType>) { ... }
              -- | ```
              genServerReqType :: F Type
              genServerReqType = do
                -- XXX: Implement this (!)
                return StringType

    genDefTypes :: String -> F (Array Type)
    genDefTypes n = (genDefTypes' =<< readProp n spec) <|> return []

    genDefTypes' :: Foreign -> F (Array Type)
    genDefTypes' def = keys def >>= traverse genDefType
      where
        genDefType :: String -> F Type
        genDefType name = do
          typ <- toType =<< readProp name def
          case typ of
            ObjectType fields -> return $ InterfaceType name [] fields
            ReferenceType ref -> return $ InterfaceType name [ref] []
            _                 -> return $ UnionType     name [typ]
