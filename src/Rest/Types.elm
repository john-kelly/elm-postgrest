module Rest.Types exposing (..)

{-| Rest.Types
# Types
@docs Resource, Property, RestRequest, Condition, Filter, OrderBy
# Functions
@docs unwrapResource, unwrapRestRequest
-}


{-| -}
type Resource schema
    = Resource String schema


{-| helper to assist with opaque Resource type
-}
unwrapResource : Resource schema -> ( String, schema )
unwrapResource resource =
    case resource of
        Resource name schema ->
            ( name, schema )


{-| -}
type RestRequest schema
    = RestRequest
        { properties : List Property
        , filters : List Filter
        , orders : List OrderBy
        , limits : List ( Resource schema, Int )
        , offset : Int
        , singular : Bool
        , suppressCount : Bool
        , verb : String
        , resource : Resource schema
        , url : String
        }


{-| helper to assist with opaque RestRequest type
-}
unwrapRestRequest :
    RestRequest schema
    -> { properties : List Property
       , filters : List Filter
       , orders : List OrderBy
       , limits : List ( Resource schema, Int )
       , offset : Int
       , singular : Bool
       , suppressCount : Bool
       , verb : String
       , resource : Resource schema
       , url : String
       }
unwrapRestRequest request =
    case request of
        RestRequest record ->
            record


{-| -}
type OrderBy
    = Ascending Property
    | Descending Property


{-| -}
type Property
    = SimpleProperty String
    | NestedResource String (List Property)


{-| -}
type
    Condition
    -- TODO support generic types https://github.com/elm-lang/core/issues/657
    = LikeFilter Property String
    | ILikeFilter Property String
    | EqFilter Property String
    | GteFilter Property String
    | GtFilter Property String
    | LteFilter Property String
    | LtFilter Property String
    | InFilter Property (List String)
    | IsFilter Property String
    | ContainsFilter Property String


{-| -}
type Filter
    = Filter Bool Condition
