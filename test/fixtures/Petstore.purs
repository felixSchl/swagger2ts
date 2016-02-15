module Test.Fixtures.Petstore where

import Data.Maybe
import Swagger.TypeGen

expected :: SwaggerTypes
expected = Swagger.TypeGen.SwaggerTypes{
  parameters: [],
  definitions: [],
  responses: [],
   client:[
      Swagger.TypeGen.ReqRes {
         request:Swagger.TypeGen.InterfaceType "FindPetsRequest" [] [
            Swagger.TypeGen.Field {
               default:Data.Maybe.Nothing,
               name:"tags",
               required:false,
               type:Swagger.TypeGen.ArrayType Swagger.TypeGen.StringType
            },
            Swagger.TypeGen.Field {
               default:Data.Maybe.Nothing,
               name:"limit",
               required:false,
               type:Swagger.TypeGen.NumberType
            }
         ],
         response:Swagger.TypeGen.StringType
      },
      Swagger.TypeGen.ReqRes {
         request:Swagger.TypeGen.InterfaceType "AddPetRequest" [] [
            Swagger.TypeGen.Field {
               default:Data.Maybe.Nothing,
               name:"pet",
               required:true,
               type:Swagger.TypeGen.ReferenceType "definitions.NewPet"
            }
         ],
         response:Swagger.TypeGen.StringType
      },
      Swagger.TypeGen.ReqRes {
         request:Swagger.TypeGen.InterfaceType "Find pet by idRequest" [] [
            Swagger.TypeGen.Field {
               default:Data.Maybe.Nothing,
               name:"id",
               required:true,
               type:Swagger.TypeGen.NumberType
            }
         ],
         response:Swagger.TypeGen.StringType
      },
      Swagger.TypeGen.ReqRes {
         request:Swagger.TypeGen.InterfaceType "DeletePetRequest" [] [
            Swagger.TypeGen.Field {
               default:Data.Maybe.Nothing,
               name:"id",
               required:true,
               type:Swagger.TypeGen.NumberType
            }
         ],
         response:Swagger.TypeGen.StringType
      }
   ],
   server:[
      Swagger.TypeGen.ReqRes {
         request:Swagger.TypeGen.InterfaceType "FindPetsRequest" [] [
            Swagger.TypeGen.Field {
               default:Data.Maybe.Nothing,
               name:"query",
               required:false,
               type:Swagger.TypeGen.ObjectType [
                  Swagger.TypeGen.Field {
                     default:Data.Maybe.Nothing,
                     name:"limit",
                     required:false,
                     type:Swagger.TypeGen.NumberType
                  },
                  Swagger.TypeGen.Field {
                     default:Data.Maybe.Nothing,
                     name:"tags",
                     required:false,
                     type:Swagger.TypeGen.ArrayType Swagger.TypeGen.StringType
                  }
               ]
            }
         ],
         response:Swagger.TypeGen.StringType
      },
      Swagger.TypeGen.ReqRes {
         request:Swagger.TypeGen.InterfaceType "AddPetRequest" [] [
            Swagger.TypeGen.Field {
               default:Data.Maybe.Nothing,
               name:"body",
               required:false,
               type:Swagger.TypeGen.ObjectType [
                  Swagger.TypeGen.Field {
                     default:Data.Maybe.Nothing,
                     name:"pet",
                     required:true,
                     type:Swagger.TypeGen.ReferenceType "definitions.NewPet"
                  }
               ]
            }
         ],
         response:Swagger.TypeGen.StringType
      },
      Swagger.TypeGen.ReqRes {
         request:Swagger.TypeGen.InterfaceType "Find pet by idRequest" [] [
            Swagger.TypeGen.Field {
               default:Data.Maybe.Nothing,
               name:"pathParams",
               required:false,
               type:Swagger.TypeGen.ObjectType [
                  Swagger.TypeGen.Field {
                     default:Data.Maybe.Nothing,
                     name:"id",
                     required:true,
                     type:Swagger.TypeGen.NumberType
                  }
               ]
            }
         ],
         response:Swagger.TypeGen.StringType
      },
      Swagger.TypeGen.ReqRes {
         request:Swagger.TypeGen.InterfaceType "DeletePetRequest" [] [
            Swagger.TypeGen.Field {
               default:Data.Maybe.Nothing,
               name:"pathParams",
               required:false,
               type:Swagger.TypeGen.ObjectType [
                  Swagger.TypeGen.Field {
                     default:Data.Maybe.Nothing,
                     name:"id",
                     required:true,
                     type:Swagger.TypeGen.NumberType
                  }
               ]
            }
         ],
         response:Swagger.TypeGen.StringType
      }
   ]
}
