module Query.Types exposing (..)

{-| Query.Types
# Types
@docs Schema, Field, Query, Condition, Filter, OrderBy
# Functions
@docs unwrapSchema, unwrapQuery
-}


{-| -}
type Schema shape
    = Schema String shape


{-| helper to assist with opaque Schema type
-}
unwrapSchema : Schema shape -> ( String, shape )
unwrapSchema schema =
    case schema of
        Schema name shape ->
            ( name, shape )


{-| -}
type Query shape
    = Query
        { fields : List Field
        , filters : List Filter
        , orders : List OrderBy
        , limit : Maybe Int
        , offset : Maybe Int
        , singular : Bool
        , suppressCount : Bool
        , verb : String
        , schema : Schema shape
        , url : String
        }


{-| helper to assist with opaque Query type
-}
unwrapQuery :
    Query shape
    -> { fields : List Field
       , filters : List Filter
       , orders : List OrderBy
       , limit : Maybe Int
       , offset : Maybe Int
       , singular : Bool
       , suppressCount : Bool
       , verb : String
       , schema : Schema shape
       , url : String
       }
unwrapQuery request =
    case request of
        Query record ->
            record


{-| -}
type OrderBy
    = Ascending Field
    | Descending Field


{-| -}
type Field
    = SimpleField String
    | NestedField String (List Field)


{-| -}
type
    Condition
    -- TODO support generic types https://github.com/elm-lang/core/issues/657
    = Like Field String
    | ILike Field String
    | Eq Field String
    | Gte Field String
    | Gt Field String
    | Lte Field String
    | Lt Field String
    | In Field (List String)
    | Is Field String
    | Contains Field String


{-| -}
type Filter
    = Filter Bool Condition
