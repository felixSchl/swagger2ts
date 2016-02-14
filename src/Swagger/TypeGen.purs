module Swagger.TypeGen where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Foreign
import Data.Bifunctor (bimap)
import Data.Foreign.Index
import Data.Foreign.Keys
import Data.Foreign.Class
import Debug.Trace
import Node.Buffer hiding (read, readString)
import Data.Generic
import Control.Bind ((=<<))
import Data.Array (foldM, (:), concatMap, length, replicate)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Foldable (foldl, any, intercalate)
import Data.String (toUpper, replace, fromCharArray, uncons, fromChar)
import qualified Data.String.Regex as R
import qualified Data.StrMap as Map
import Data.Tuple (Tuple (..))

foreign import parseYaml :: Buffer -> Foreign

newtype Path = Path {
  path :: String
, ops  :: Array Operation
}

newtype Operation = Operation {
  id     :: Maybe String
, name   :: String
, params :: Array Param
, path   :: String
, method :: String
}

newtype Param = Param {
  loc      :: String
, name     :: String
, required :: Boolean
, type     :: Type
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
  | ClassType Name
  | UnionType Name Types
  | InterfaceType Name Extends Fields

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

renderType :: Type -> String
renderType = render 0
  where
    indent :: Int -> String
    indent i = fromCharArray $ replicate (i * 2) ' '

    render :: Int -> Type -> String
    render _ StringType           = "string"
    render _ BooleanType          = "boolean"
    render _ NumberType           = "number"
    render _ (ReferenceType name) = name
    render i (ArrayType typ)      = render i typ ++ "[]"
    render i (ObjectType fields)  =
      if length fields == 0
        then "{}"
        else "{\n"
          ++ (intercalate "\n" $ fields <#> \(Field f) ->
                indent i
              ++ f.name
              ++ (if f.required then "" else "?")
              ++ ": "
              ++ render (i + 1) f.type
              ++ ";")
          ++ "\n"
          ++ indent (i - 1) ++ "}"
    render i (InterfaceType n xs fs) =
      "interface " ++ n ++ " "
        ++ (render (i + 1) $ ObjectType fs)

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
      (do
        -- Upper-case first letter
        id <- opId
        return $ maybe ""
                       (\({ head, tail }) ->
                        (toUpper $ fromChar head) ++ tail)
                       (uncons id))

  params <- do
    traverse readParam =<< do
      (readArray =<< readProp "parameters" op) <|> return []

  return $ Operation {
    id:     opId
  , name:   name
  , method: method
  , path:   path
  , params: params
  }

newtype ReqRes = ReqRes {
  request  :: Type
, response :: Type
}

newtype SwaggerTypes = SwaggerTypes {
  server :: Array ReqRes
, client :: Array ReqRes
}

derive instance genericReqRes :: Generic ReqRes

instance showReqRes :: Show ReqRes where
  show = gShow

instance eqReqRes :: Eq ReqRes where
  eq = gEq

derive instance genericSwaggerTypes :: Generic SwaggerTypes

instance showSwaggerTypes :: Show SwaggerTypes where
  show = gShow

instance eqSwaggerTypes :: Eq SwaggerTypes where
  eq = gEq

-- Derive type information from a swagger spec.
generateTypes :: Foreign -> F SwaggerTypes
generateTypes spec = do
  paths <- readPaths

  definitions <- genDefTypes "definitions"
  parameters  <- genDefTypes "parameters"
  responses   <- genDefTypes "responses"

  let ops = concatMap (\(Path path) -> path.ops) paths
      gen = genTypes definitions parameters responses

  return $ SwaggerTypes {
    server: ops <#> gen genServerRequestType genResponseType
  , client: ops <#> gen genClientRequestType genResponseType
  }

  where

    genTypes d p r freq fres op = ReqRes { request:  freq d p r op
                                         , response: fres d p r op
                                         }

    genClientRequestType :: Array Type -> Array Type -> Array Type
                         -> Operation
                         -> Type
    genClientRequestType _ _ _ (Operation op) =
      let fields = op.params <#> \(Param p) ->
                    Field {
                      name:     p.name
                    , required: p.required
                    , type:     p.type
                    , default:  Nothing
                    }
       in InterfaceType (op.name ++ "Request") [] fields

    genServerRequestType :: Array Type -> Array Type -> Array Type
                         -> Operation
                         -> Type
    genServerRequestType _ _ _ (Operation op) =
      -- Reduce parameters into a map of `location -> types`.
      -- This reduction will select a single 'body' (if any) or a single
      -- 'formData', in an undefined fashion. Other parameters are merged.
      -- Refer to the swagger specification:
      -- http://swagger.io/specification/#parameterObject
      let fields = flip Map.foldMap
                      (ObjectType <$> (foldl step Map.empty op.params))
                      (\loc typ -> [
                        Field {
                          name:     loc
                        , required: false -- XXX: Determine based on `v`
                        , type:     typ
                        , default:  Nothing
                        }
                      ])
       in InterfaceType (op.name ++ "Request") [] fields
      where
        step m (Param p) =
          if (p.loc == "body") || (p.loc == "formData")
            then Map.insert p.loc (unwrapField f) m
            else Map.alter resolveParams p.loc m
          where
            unwrapField (Field { type: (ObjectType fs) }) = fs
            unwrapField f = [f]

            resolveParams Nothing   = Just [f]
            resolveParams (Just fs) = Just (f:fs)
            f = Field {
              name:     p.name
            , required: p.required
            , type:     p.type
            , default:  Nothing
            }

    genResponseType :: Array Type -> Array Type -> Array Type
                          -> Operation
                          -> Type
    genResponseType _ _ _ _ = StringType -- XXX

    readPaths :: F (Array Path)
    readPaths = do
      paths    <- readProp "paths" spec
      pathKeys <- keys paths
      flip traverse pathKeys \pathKey -> do
        -- TODO: Read shared parameters
        path   <- readProp pathKey paths
        opKeys <- keys path
        ops    <- flip traverse opKeys \opKey -> do
                    readOp opKey pathKey =<< readProp opKey path

        return $ Path {
          path: pathKey
        , ops:  ops
        }

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
