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
        { fields : List (Field shape)
        , filters : List (Filter shape)
        , orders : List (OrderBy shape)
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
    -> { fields : List (Field shape)
       , filters : List (Filter shape)
       , orders : List (OrderBy shape)
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
type OrderBy shape
    = Ascending (Field shape)
    | Descending (Field shape)


{-| -}
type Field shape
    = SimpleField String
    | NestedField String (Query shape)


{-| -}
type
    Condition shape
    -- TODO support generic types https://github.com/elm-lang/core/issues/657
    = Like (Field shape) String
    | ILike (Field shape) String
    | Eq (Field shape) String
    | Gte (Field shape) String
    | Gt (Field shape) String
    | Lte (Field shape) String
    | Lt (Field shape) String
    | In (Field shape) (List String)
    | Is (Field shape) String
    | Contains (Field shape) String


{-| -}
type Filter shape
    = Filter Bool (Condition shape)
