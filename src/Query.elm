module Query
    exposing
        ( Schema
        , Query
        , Field
        , OrderBy
        , Filter
        , Settings
        , defaultSettings
        , schema
        , field
        , query
        , subQuery
        , select
        , order
        , filter
        , like
        , eq
        , gte
        , gt
        , lte
        , lt
        , ilike
        , in'
        , is
        , not'
        , asc
        , desc
        )

{-| -}

import Json.Decode as Decode
import Query.Types exposing (..)


type alias Schema s =
    Query.Types.Schema s


type alias Query s =
    Query.Types.Query s


type alias Field =
    Query.Types.Field


type alias OrderBy =
    Query.Types.OrderBy


type alias Filter =
    Query.Types.Filter


{-| -}
type alias Settings =
    { count : Bool
    , singular : Bool
    , limit : Maybe Int
    , offset : Maybe Int
    }


{-| -}
defaultSettings : Settings
defaultSettings =
    { count = False
    , singular = False
    , limit = Nothing
    , offset = Nothing
    }


{-| thanks lukewestby
https://github.com/elm-lang/core/issues/657
-}
coerceToString : a -> String
coerceToString value =
    let
        stringValue =
            toString value
    in
        stringValue
            |> Decode.decodeString Decode.string
            |> Result.withDefault stringValue


{-| -}
schema : String -> s -> Schema s
schema =
    Schema


{-| -}
field : String -> Field
field =
    Simple


{-| -}
query : Schema s -> Query s
query schema =
    let
        ( name, shape ) =
            unwrapSchema schema
    in
        Query name
            shape
            { select = []
            , filter = []
            , order = []
            }


{-| -}
subQuery : Query s -> a -> Field
subQuery query =
    let
        ( name, _, params ) =
            unwrapQuery query
    in
        always <| Nested name params


{-| -}
select : List (s -> Field) -> Query s -> Query s
select selects query =
    let
        -- addSelects : s -> QueryParams -> QueryParams
        -- https://github.com/elm-lang/elm-compiler/issues/1214
        addSelects shape params =
            { params
                | select = params.select ++ List.map (\fn -> fn shape) selects
            }
    in
        mapQueryParams addSelects query


{-| -}
order : List (s -> OrderBy) -> Query s -> Query s
order orders query =
    let
        -- addOrders : s -> QueryParams -> QueryParams
        addOrders shape params =
            { params
                | order = params.order ++ List.map (\fn -> fn shape) orders
            }
    in
        mapQueryParams addOrders query


{-| -}
filter : List (s -> Filter) -> Query s -> Query s
filter filters query =
    let
        -- addFilters : s -> QueryParams -> QueryParams
        addFilters shape params =
            { params
                | filter = params.filter ++ List.map (\fn -> fn shape) filters
            }
    in
        mapQueryParams addFilters query


mapQueryParams : (s -> QueryParams -> QueryParams) -> Query s -> Query s
mapQueryParams fn query =
    let
        ( name, shape, params ) =
            unwrapQuery query
    in
        Query name shape (fn shape params)


singleValueFilterFn :
    (String -> Condition)
    -> a
    -> (s -> Field)
    -> (s -> Filter)
singleValueFilterFn condCtor condArg fieldAccessor =
    let
        -- shapeToFilter : s -> Filter
        shapeToFilter shape =
            Filter False
                (condCtor (coerceToString condArg))
                (fieldAccessor shape)
    in
        shapeToFilter


{-| -}
like : String -> (s -> Field) -> (s -> Filter)
like =
    singleValueFilterFn Like


{-| -}
eq : a -> (s -> Field) -> (s -> Filter)
eq =
    singleValueFilterFn Eq


{-| -}
gte : a -> (s -> Field) -> (s -> Filter)
gte =
    singleValueFilterFn Gte


{-| -}
gt : a -> (s -> Field) -> (s -> Filter)
gt =
    singleValueFilterFn Gt


{-| -}
lte : a -> (s -> Field) -> (s -> Filter)
lte =
    singleValueFilterFn Lte


{-| -}
lt : a -> (s -> Field) -> (s -> Filter)
lt =
    singleValueFilterFn Lt


{-| -}
ilike : String -> (s -> Field) -> (s -> Filter)
ilike =
    singleValueFilterFn ILike


{-| -}
in' : List a -> (s -> Field) -> (s -> Filter)
in' condArgs fieldAccessor =
    let
        shapeToFilter shape =
            Filter False
                (In (List.map coerceToString condArgs))
                (fieldAccessor shape)
    in
        shapeToFilter


{-| -}
is : a -> (s -> Field) -> (s -> Filter)
is =
    singleValueFilterFn Is


{-| -}
not' :
    (a -> (s -> Field) -> (s -> Filter))
    -> a
    -> (s -> Field)
    -> (s -> Filter)
not' filterAccessorCtor val fieldAccessor =
    let
        filterAccessor =
            filterAccessorCtor val fieldAccessor

        shapeToNegatedFilter shape =
            case filterAccessor shape of
                Filter negated cond field ->
                    Filter (not negated) cond field
    in
        shapeToNegatedFilter


{-| -}
asc : (s -> Field) -> (s -> OrderBy)
asc fieldAccessor =
    (\shape -> Asc (fieldAccessor shape))


{-| -}
desc : (s -> Field) -> (s -> OrderBy)
desc fieldAccessor =
    (\shape -> Desc (fieldAccessor shape))
