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
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Foldable (any)
import Data.String (toUpper, replace)
import qualified Data.String.Regex as R

foreign import parseYaml :: Buffer -> Foreign

newtype Param = Param {
  loc      :: String
, name     :: String
, required :: Boolean
, type     :: Type
}

newtype Operation = Operation {
  id     :: Maybe String
, name   :: String
, params :: Array Param
}

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

derive instance paramField :: Generic Param
instance showParam :: Show Param where
  show = gShow

derive instance operationField :: Generic Operation
instance showOperation :: Show Operation where
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

readParam :: Foreign -> F Param
readParam param = do
  loc      <- readString =<< readProp "in"   param
  name     <- readString =<< readProp "name" param
  required <- case loc of
    "path" -> return true
    _      -> (readBoolean =<< readProp "required" param)
                <|> return false

  -- Determine the "type" of the parameter. Unfortunately, we cannot
  -- simply use `toType` because of how the specification is layed
  -- out. Reference: http://swagger.io/specification/#parameterObject
  -- The type check for non-body parameters below is exhaustive as far
  -- as the specifaction is concerned.

  typ <- case loc of
    "body" -> toType =<< readProp "schema" param
    _      -> do
      typ <- readString =<< readProp "type" param
      case typ of
        "string"  -> return StringType
        "number"  -> return NumberType
        "integer" -> return NumberType
        "boolean" -> return BooleanType
        "file"    -> Left $ JSONError "FILE NOT IMPLEMENTED"
        "array"   -> ArrayType <$> do toType =<< readProp "items" param
        _         -> Left $ JSONError $ "Unknown type `" ++ typ ++ "`"

  return $ Param {
    loc:      loc
  , name:     name
  , required: required
  , type:     typ
  }

readOp :: String -> String -> Foreign -> F Operation
readOp method path op = do
  opId <- do
    (Just <$> do readString =<< readProp "operationId" op)
    <|> return Nothing

  -- Construct a name for the OP.
  -- Should the optional `operationId` be given, use that. Otherwise, fall back
  -- to a concatenation of method and path.
  name <- return $ flip maybe id
      (toUpper method ++
        (R.replace (R.regex "{" $ R.parseFlags "g") "$"
          (R.replace (R.regex "}" $ R.parseFlags "g") ""
            (R.replace (R.regex "[/-]" $ R.parseFlags "g") "_"
              path))))
      opId

  params <- do
    traverse readParam =<< do
      (readArray =<< readProp "parameters" op) <|> return []

  return $ Operation {
    id:     opId
  , name:   name
  , params: params
  }

-- Derive type information from a swagger spec.
generateTypes :: Foreign -> F Unit
generateTypes spec = do
  definitions <- genDefTypes "definitions"
  parameters  <- genDefTypes "parameters"
  responses   <- genDefTypes "responses"
  operations  <- genOpTypes
  traceShowA "FOO"
  return unit

  where
    genOpTypes :: F Unit
    genOpTypes = do
      paths <- readProp "paths" spec

      -- Collect all operations per `path`
      ops <- keys paths >>= \paths' -> flip traverse paths' \path' -> do
        path <- readProp path' paths
        -- TODO: Read shared parameters
        keys path >>= \ops' -> flip traverse ops' \method -> do
          readOp method path' =<< readProp method path

      traceShowA ops

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
