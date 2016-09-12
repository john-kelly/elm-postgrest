module PostgRest
    exposing
        ( field
        , Field
        , Schema
        , Query
        , Select
        , OrderBy
        , Filter
        , schema
        , query
        , include
        , includeMany
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
        , list
        )

{-| PostgREST Query Builder!
@docs Schema, Query, Select, OrderBy, Filter, schema, field, query, include, select, order, filter, like, eq, gte, gt, lte, lt, ilike, in', is, not', asc, desc, postgRest
-}

import Dict
import Http
import Json.Decode as Decode exposing ((:=))
import String
import Task


{-| https://github.com/elm-community/json-extra/blob/master/src/Json/Decode/Extra.elm#L86
decided to just not import Json.Decode.Extra.
-}
apply : Decode.Decoder (a -> b) -> Decode.Decoder a -> Decode.Decoder b
apply =
    Decode.object2 (<|)


{-| -}
type Schema s
    = Schema String s


unwrapSchema : Schema s -> ( String, s )
unwrapSchema schema =
    case schema of
        Schema name shape ->
            ( name, shape )


{-| -}
type Query s r
    = Query String s QueryParams (Decode.Decoder r)


unwrapQuery : Query s r -> ( String, s, QueryParams, Decode.Decoder r )
unwrapQuery query =
    case query of
        Query name shape params decoder ->
            ( name, shape, params, decoder )



-- select is no longer a list of fields, we are going to have a different data
-- type to represetn a field vs a thing to query? or something like that.
-- basically this is the case b/c we need the field to be paramaterized but that
-- is just not possible if we have a list of them... becuase then each one would
-- have to be of the same type.
--
-- NOTE: HMM...
-- we could choose to NOT expose a limit setter.
-- query pokemon
--      |> limit 10
--      |> limit 3
--      |> limit 1
-- that just does not make much sense and it can override old stuff.
-- i dont like that. im not sure if it is actually a problem. i think it's fine
-- for now tho.
-- instead we could have the includeMany and the list functions just take how
-- many the user wants to limit by. i think that i like this b/c then there is
-- never confusion with the paginate api overwriting things


type alias QueryParams =
    { select : List Select
    , order : List OrderBy
    , filter : List Filter
    , limit : Maybe Int
    }


{-| -}
type Select
    = Simple String
    | Nested String QueryParams


{-|
query session Session
    |> select .id & .location

could maybe do something interesting with compound fields? or maybe it's a compound
select? not sure.... but .id & .location could output something that could be selected on
that represetnts both of those.
-}
type Field a
    = Field String (Decode.Decoder a)


{-| -}
type OrderBy
    = Asc String
    | Desc String


{-| -}
type Condition
    = Like String
    | ILike String
    | Eq String
    | Gte String
    | Gt String
    | Lte String
    | Lt String
    | In (List String)
    | Is String


{-| -}
type Filter
    = Filter Bool Condition String


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
field : String -> Decode.Decoder a -> Field a
field =
    Field


{-| -}
query : Schema s -> (a -> r) -> Query s (a -> r)
query schema recordCtor =
    let
        ( name, shape ) =
            unwrapSchema schema
    in
        Query name
            shape
            { select = [], filter = [], order = [], limit = Nothing }
            (Decode.succeed recordCtor)


{-| I have decided to NOT make nested queries into fields. If we allow queries
to be fields then an impossible state is representable (a filter of a nested field.)

-}
include : Query s2 a -> Query s1 (a -> b) -> Query s1 b
include sub query =
    let
        ( queryName, queryShape, queryParams, queryDecoder ) =
            unwrapQuery query

        ( subName, subShape, subParams, subDecoder ) =
            unwrapQuery sub

        nestedField =
            Nested subName subParams
    in
        Query queryName
            queryShape
            { queryParams | select = nestedField :: queryParams.select }
            (apply queryDecoder (subName := subDecoder))


includeMany : Maybe Int -> Query s2 a -> Query s1 (List a -> b) -> Query s1 b
includeMany limit sub query =
    let
        ( queryName, queryShape, queryParams, queryDecoder ) =
            unwrapQuery query

        ( subName, subShape, subParams, subDecoder ) =
            unwrapQuery sub

        nestedField =
            Nested subName { subParams | limit = limit }
    in
        Query queryName
            queryShape
            { queryParams | select = nestedField :: queryParams.select }
            (apply queryDecoder (subName := Decode.list subDecoder))


{-| -}
select : (s -> Field a) -> Query s (a -> b) -> Query s b
select fieldAccessor query =
    let
        ( name, shape, params, decoder ) =
            unwrapQuery query

        ( n, s, d ) =
            case fieldAccessor shape of
                Field name decoder ->
                    ( name, Simple name, decoder )
    in
        Query name
            shape
            { params | select = s :: params.select }
            (apply decoder (n := d))


{-| -}
order : List (s -> OrderBy) -> Query s r -> Query s r
order orders query =
    let
        ( name, shape, params, d ) =
            unwrapQuery query
    in
        Query name
            shape
            { params | order = params.order ++ List.map (\fn -> fn shape) orders }
            d


{-| -}
filter : List (s -> Filter) -> Query s r -> Query s r
filter filters query =
    let
        ( name, shape, params, d ) =
            unwrapQuery query
    in
        Query name
            shape
            { params | filter = params.filter ++ List.map (\fn -> fn shape) filters }
            d


singleValueFilterFn :
    (String -> Condition)
    -> a
    -> (s -> Field b)
    -> s
    -> Filter
singleValueFilterFn condCtor condArg attrAccessor shape =
    case attrAccessor shape of
        Field name _ ->
            Filter False (condCtor (coerceToString condArg)) name


{-| -}
like : String -> (s -> Field String) -> s -> Filter
like =
    singleValueFilterFn Like


{-| -}
eq : a -> (s -> Field a) -> s -> Filter
eq =
    singleValueFilterFn Eq


{-| -}
gte : a -> (s -> Field a) -> s -> Filter
gte =
    singleValueFilterFn Gte


{-| -}
gt : a -> (s -> Field a) -> s -> Filter
gt =
    singleValueFilterFn Gt


{-| -}
lte : a -> (s -> Field a) -> s -> Filter
lte =
    singleValueFilterFn Lte


{-| -}
lt : a -> (s -> Field a) -> s -> Filter
lt =
    singleValueFilterFn Lt


{-| -}
ilike : String -> (s -> Field String) -> s -> Filter
ilike =
    singleValueFilterFn ILike


{-| -}
in' : List a -> (s -> Field a) -> s -> Filter
in' condArgs attrAccessor shape =
    case attrAccessor shape of
        Field name _ ->
            Filter False (In (List.map coerceToString condArgs)) name


{-| -}
is : a -> (s -> Field a) -> s -> Filter
is =
    singleValueFilterFn Is


{-| -}
not' :
    (a -> (s -> Field a) -> (s -> Filter))
    -> a
    -> (s -> Field a)
    -> s
    -> Filter
not' filterAccessorCtor val fieldAccessor shape =
    case filterAccessorCtor val fieldAccessor shape of
        Filter negated cond fieldName ->
            Filter (not negated) cond fieldName


{-| -}
asc : (s -> Field a) -> s -> OrderBy
asc fieldAccessor shape =
    case fieldAccessor shape of
        Field name _ ->
            Asc name


{-| -}
desc : (s -> Field a) -> s -> OrderBy
desc fieldAccessor shape =
    case fieldAccessor shape of
        Field name _ ->
            Desc name


{-| -}
list : Maybe Int -> String -> Query s r -> Task.Task Http.Error (List r)
list limit url query =
    let
        ( name, shape, params, decoder ) =
            unwrapQuery query

        limitedQuery =
            Query name shape { params | limit = limit } decoder

        settings =
            { count = False
            , singular = False
            , offset = Nothing
            }
    in
        -- according to ~20:00 fromJson may be too limiting in terms of error
        -- handling https://www.dailydrip.com/topics/elm/drips/server-side-validations
        -- this is b/c you can access the body of the response when there is an
        -- error. the solution to this in http builder is to basically just have
        -- an error type that comes with the Response and we can read it in a
        -- similar fashion to reading the json body.
        postgRest settings url limitedQuery
            |> Http.send Http.defaultSettings
            |> Http.fromJson (Decode.list decoder)


{-| -}
retrieve : String -> Query s r -> Task.Task Http.Error r
retrieve url query =
    let
        ( _, _, _, decoder ) =
            unwrapQuery query

        settings =
            { count = False
            , singular = True
            , offset = Nothing
            }
    in
        postgRest settings url query
            |> Http.send Http.defaultSettings
            |> Http.fromJson decoder


{-| TODO need to change the shape of the response to resemble a page! (include count and page number and such)
-- maybe even a url to the next page? that might be cool.
-}
paginate : String -> Int -> Int -> Query s r -> Task.Task Http.Error (List r)
paginate url pageNumber pageSize query =
    let
        ( name, shape, params, decoder ) =
            unwrapQuery query

        limitedQuery =
            Query name shape { params | limit = Just pageSize } decoder

        settings =
            { count = True
            , singular = False
            , offset = Just (pageNumber * pageSize)
            }
    in
        postgRest settings url query
            |> Http.send Http.defaultSettings
            |> Http.fromJson (Decode.list decoder)


type alias Settings =
    { count : Bool
    , singular : Bool
    , offset : Maybe Int
    }


postgRest : Settings -> String -> Query s r -> Http.Request
postgRest settings url query =
    let
        { count, singular, offset } =
            settings

        ( name, _, params, _ ) =
            unwrapQuery query

        trailingSlashUrl =
            if String.right 1 url == "/" then
                url
            else
                url ++ "/"

        queryUrl =
            [ fieldsToKeyValue params.select
            , params
                |> labelOrders ""
                |> labeledOrdersToKeyValue
            , params
                |> labelFilters ""
                |> labeledFiltersToKeyValues
            , offsetToKeyValue offset
            , limitToKeyValues params.limit
              -- TODO need to run this on nested as well.
            ]
                |> List.foldl (++) []
                |> Http.url (trailingSlashUrl ++ name)

        pluralityHeader =
            if singular then
                [ ( "Prefer", "plurality=singular" ) ]
            else
                []

        countHeader =
            if not count then
                [ ( "Prefer", "count=none" ) ]
            else
                []

        headers =
            pluralityHeader ++ countHeader
    in
        { verb = "GET"
        , headers = headers
        , url = queryUrl
        , body = Http.empty
        }


fieldsToKeyValue : List Select -> List ( String, String )
fieldsToKeyValue fields =
    let
        fieldToString : Select -> String
        fieldToString field =
            case field of
                Simple name ->
                    name

                Nested name { select } ->
                    name ++ "{" ++ fieldsToString select ++ "}"

        fieldsToString : List Select -> String
        fieldsToString fields =
            case fields of
                [] ->
                    "*"

                _ ->
                    fields
                        |> List.map fieldToString
                        |> String.join ","
    in
        case fields of
            [] ->
                []

            _ ->
                [ ( "select", fieldsToString fields ) ]


labelFilters : String -> QueryParams -> List ( String, Filter )
labelFilters prefix params =
    let
        labelWithPrefix =
            (,) prefix

        labeledFilters =
            List.map labelWithPrefix params.filter

        labelNestedFilters field =
            case field of
                Simple _ ->
                    Nothing

                Nested nestedName nestedParams ->
                    Just (labelFilters (prefix ++ nestedName ++ ".") nestedParams)

        labeledNestedFilters =
            params.select
                |> List.filterMap labelNestedFilters
                |> List.concat
    in
        labeledFilters ++ labeledNestedFilters


labeledFiltersToKeyValues : List ( String, Filter ) -> List ( String, String )
labeledFiltersToKeyValues filters =
    let
        contToString : Condition -> String
        contToString cond =
            case cond of
                Like str ->
                    "like." ++ str

                Eq str ->
                    "eq." ++ str

                Gte str ->
                    "gte." ++ str

                Gt str ->
                    "gt." ++ str

                Lte str ->
                    "lte." ++ str

                Lt str ->
                    "lt." ++ str

                ILike str ->
                    "ilike." ++ str

                In list ->
                    "in." ++ String.join "," list

                Is str ->
                    "is." ++ str

        filterToKeyValue : ( String, Filter ) -> ( String, String )
        filterToKeyValue ( prefix, filter ) =
            case filter of
                Filter True cond key ->
                    ( prefix ++ key, "not." ++ contToString cond )

                Filter False cond key ->
                    ( prefix ++ key, contToString cond )
    in
        List.map filterToKeyValue filters


labelOrders : String -> QueryParams -> List ( String, OrderBy )
labelOrders prefix params =
    let
        labelWithPrefix =
            (,) prefix

        labeledOrders =
            List.map labelWithPrefix params.order

        labelNestedOrders field =
            case field of
                Simple _ ->
                    Nothing

                Nested nestedName nestedParams ->
                    Just (labelOrders (prefix ++ nestedName ++ ".") nestedParams)

        labeledNestedOrders =
            params.select
                |> List.filterMap labelNestedOrders
                |> List.concat
    in
        labeledOrders ++ labeledNestedOrders


labeledOrdersToKeyValue : List ( String, OrderBy ) -> List ( String, String )
labeledOrdersToKeyValue orders =
    let
        labeledOrderToKeyValue : ( String, List OrderBy ) -> Maybe ( String, String )
        labeledOrderToKeyValue ( prefix, orders ) =
            case orders of
                [] ->
                    Nothing

                _ ->
                    Just
                        ( prefix ++ "order"
                        , orders
                            |> List.map orderToString
                            |> String.join ","
                        )

        orderToString : OrderBy -> String
        orderToString order =
            case order of
                Asc name ->
                    name ++ ".asc"

                Desc name ->
                    name ++ ".desc"
    in
        orders
            |> List.foldr
                (\( prefix, order ) dict ->
                    Dict.update prefix
                        (\maybeOrders ->
                            case maybeOrders of
                                Nothing ->
                                    Just [ order ]

                                Just os ->
                                    Just (order :: os)
                        )
                        dict
                )
                Dict.empty
            |> Dict.toList
            |> List.filterMap labeledOrderToKeyValue


offsetToKeyValue : Maybe Int -> List ( String, String )
offsetToKeyValue maybeOffset =
    case maybeOffset of
        Nothing ->
            []

        Just offset ->
            [ ( "offset", toString offset ) ]


limitToKeyValues : Maybe Int -> List ( String, String )
limitToKeyValues maybeLimit =
    case maybeLimit of
        Nothing ->
            []

        Just limit ->
            [ ( "limit", toString limit ) ]
