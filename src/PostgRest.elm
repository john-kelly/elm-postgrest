module PostgRest
    exposing
        ( field
        , Field
        , Resource
        , Query
        , OrderBy
        , Filter
        , resource
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
        , retrieve
        , paginate
        , Page
        )

{-|

# PostgREST Query Builder

To understand how to use this library it's best to take a look at the [examples](https://github.com/john-kelly/elm-postgrest/blob/master/examples/Main.elm)

## Types
@docs Field, Resource, Query, Select, OrderBy, Filter

## Field and resource
@docs field, resource

## Http Tasks
Tasks used to make Http requests

@docs list, retrieve

## Queries
@docs query, include, includeMany, select, order, filter

## Order
@docs asc, desc

## Filters
@docs like, ilike, eq, gte, gt, lte, lt, in', is, not'
-}

import Dict
import Http
import Json.Decode as Decode exposing ((:=))
import String
import Task
import PostgRest.Http


{-| Copy pasta of Json.Decode.Extra.apply
https://github.com/elm-community/json-extra/blob/master/src/Json/Decode/Extra.elm#L86
-}
apply : Decode.Decoder (a -> b) -> Decode.Decoder a -> Decode.Decoder b
apply =
    Decode.object2 (<|)


{-| -}
type Resource shape
    = Resource String shape


{-| -}
type Query s r
    = Query String s QueryParams (Decode.Decoder r)


{-| -}
type alias QueryParams =
    -- TODO: in terms of both api design and implementation, it might be a good idea
    -- to represent limit as a Limit type. we can create a nice api for the user
    -- like so: |> list (limit 5) "http://postgrest.herokuapp.com/"
    -- TODO: select should never be empty, so we're going to want to switch the
    -- implementation to a  { first: ..., rest: ... } eventually. For now, this
    -- is a known bug that hopefully people dont run into.
    { select : List Select
    , order : List OrderBy
    , filter : List Filter
    , limit : Maybe Int
    }


{-| -}
type alias Page a =
    { data : List a
    , count : Int
    }


{-| -}
type Select
    = Simple String
    | Nested String QueryParams


{-| -}
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
resource : String -> s -> Resource s
resource =
    Resource


{-| -}
field : String -> Decode.Decoder a -> Field a
field =
    Field


{-| -}
query : Resource s -> (a -> r) -> Query s (a -> r)
query (Resource name shape) recordCtor =
    Query name
        shape
        { select = [], filter = [], order = [], limit = Nothing }
        (Decode.succeed recordCtor)


{-| -}
include : Query s2 a -> Query s1 (a -> b) -> Query s1 b
include (Query subName subShape subParams subDecoder) (Query queryName queryShape queryParams queryDecoder) =
    Query queryName
        queryShape
        { queryParams | select = Nested subName subParams :: queryParams.select }
        (apply queryDecoder (subName := subDecoder))


{-| -}
includeMany : Maybe Int -> Query s2 a -> Query s1 (List a -> b) -> Query s1 b
includeMany limit (Query subName subShape subParams subDecoder) (Query queryName queryShape queryParams queryDecoder) =
    Query queryName
        queryShape
        { queryParams | select = Nested subName { subParams | limit = limit } :: queryParams.select }
        (apply queryDecoder (subName := Decode.list subDecoder))


{-| -}
select : (s -> Field a) -> Query s (a -> b) -> Query s b
select fieldAccessor (Query name shape params decoder) =
    case fieldAccessor shape of
        Field fieldName fieldDecoder ->
            Query name
                shape
                { params | select = Simple fieldName :: params.select }
                (apply decoder (fieldName := fieldDecoder))


{-| -}
order : List (s -> OrderBy) -> Query s r -> Query s r
order orders (Query name shape params decoder) =
    Query name
        shape
        { params | order = params.order ++ List.map (\fn -> fn shape) orders }
        decoder


{-| Apply filters to a query
-}
filter : List (s -> Filter) -> Query s r -> Query s r
filter filters (Query name shape params decoder) =
    Query name
        shape
        { params | filter = params.filter ++ List.map (\fn -> fn shape) filters }
        decoder


{-| -}
singleValueFilterFn : (String -> Condition) -> a -> (s -> Field b) -> s -> Filter
singleValueFilterFn condCtor condArg attrAccessor shape =
    case attrAccessor shape of
        Field name _ ->
            Filter False (condCtor (coerceToString condArg)) name


{-|
Simple [pattern matching](https://www.postgresql.org/docs/9.0/static/functions-matching.html)
-}
like : String -> (s -> Field String) -> s -> Filter
like =
    singleValueFilterFn Like


{-| Case-insensitive `like`
-}
ilike : String -> (s -> Field String) -> s -> Filter
ilike =
    singleValueFilterFn ILike


{-| Equals
-}
eq : a -> (s -> Field a) -> s -> Filter
eq =
    singleValueFilterFn Eq


{-| Greater than or equals
-}
gte : a -> (s -> Field a) -> s -> Filter
gte =
    singleValueFilterFn Gte


{-| Greater than
-}
gt : a -> (s -> Field a) -> s -> Filter
gt =
    singleValueFilterFn Gt


{-| Less than or equals
-}
lte : a -> (s -> Field a) -> s -> Filter
lte =
    singleValueFilterFn Lte


{-| Less than
-}
lt : a -> (s -> Field a) -> s -> Filter
lt =
    singleValueFilterFn Lt


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
not' : (a -> (s -> Field a) -> (s -> Filter)) -> a -> (s -> Field a) -> s -> Filter
not' filterAccessorCtor val fieldAccessor shape =
    case filterAccessorCtor val fieldAccessor shape of
        Filter negated cond fieldName ->
            Filter (not negated) cond fieldName


{-| Ascending
-}
asc : (s -> Field a) -> s -> OrderBy
asc fieldAccessor shape =
    case fieldAccessor shape of
        Field name _ ->
            Asc name


{-| Descending
-}
desc : (s -> Field a) -> s -> OrderBy
desc fieldAccessor shape =
    case fieldAccessor shape of
        Field name _ ->
            Desc name



-- naming for these functions is based off of:
-- http://www.django-rest-framework.org/api-guide/generic-views/#retrieveupdateapiview


{-| Takes `limit`, `url` and a `query`, returning a list of objects from database on success and Http.Error otherwise
-}
list : Maybe Int -> String -> Query s r -> Task.Task Http.Error (List r)
list limit url (Query name _ params decoder) =
    let
        settings =
            { count = False
            , singular = False
            , offset = Nothing
            }
    in
        toHttpRequest settings url name { params | limit = limit }
            |> Http.send Http.defaultSettings
            |> Http.fromJson (Decode.list decoder)


{-| Takes `url` and a `query`, returning the first object from database on success and Http.Error otherwise
-}
retrieve : String -> Query s r -> Task.Task Http.Error r
retrieve url (Query name _ params decoder) =
    let
        settings =
            { count = False
            , singular = True
            , offset = Nothing
            }
    in
        toHttpRequest settings url name params
            |> Http.send Http.defaultSettings
            |> Http.fromJson decoder


{-| -}
paginate : { pageNumber : Int, pageSize : Int } -> String -> Query s r -> Task.Task Http.Error (Page r)
paginate { pageNumber, pageSize } url (Query name _ params decoder) =
    let
        settings =
            -- NOTE: pageNumber is NOT 0 indexed. the first page is 1.
            { count = True
            , singular = False
            , offset = Just ((pageNumber - 1) * pageSize)
            }
    in
        toHttpRequest settings url name { params | limit = Just pageSize }
            |> Http.send Http.defaultSettings
            |> PostgRest.Http.fromPage (Decode.list decoder)


type alias Settings =
    { count : Bool
    , singular : Bool
    , offset : Maybe Int
    }


{-| -}
toHttpRequest : Settings -> String -> String -> QueryParams -> Http.Request
toHttpRequest settings url name params =
    let
        { count, singular, offset } =
            settings

        trailingSlashUrl =
            if String.right 1 url == "/" then
                url
            else
                url ++ "/"

        ( labeledOrders, labeledFilters, labeledLimits ) =
            labelParams params

        queryUrl =
            [ selectsToKeyValue params.select
            , labeledFiltersToKeyValues labeledFilters
            , labeledOrdersToKeyValue labeledOrders
            , labeledLimitsToKeyValue labeledLimits
            , offsetToKeyValue offset
            ]
                |> List.foldl (++) []
                |> Http.url (trailingSlashUrl ++ name)

        pluralityHeader =
            if singular then
                [ ( "Prefer", "plurality=singular" ) ]
            else
                []

        countHeader =
            if count then
                -- https://github.com/begriffs/postgrest/pull/700
                [ ( "Prefer", "count=exact" ) ]
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


{-| -}
selectsToKeyValue : List Select -> List ( String, String )
selectsToKeyValue fields =
    let
        selectToString : Select -> String
        selectToString field =
            case field of
                Simple name ->
                    name

                Nested name { select } ->
                    name ++ "{" ++ selectsToString select ++ "}"

        selectsToString : List Select -> String
        selectsToString fields =
            case fields of
                [] ->
                    ""

                _ ->
                    fields
                        |> List.map selectToString
                        |> String.join ","
    in
        case fields of
            [] ->
                []

            _ ->
                [ ( "select", selectsToString fields ) ]


{-| -}
offsetToKeyValue : Maybe Int -> List ( String, String )
offsetToKeyValue maybeOffset =
    case maybeOffset of
        Nothing ->
            []

        Just offset ->
            [ ( "offset", toString offset ) ]


{-| -}
labelParams' : String -> QueryParams -> ( List ( String, OrderBy ), List ( String, Filter ), List ( String, Maybe Int ) )
labelParams' prefix params =
    let
        labelWithPrefix : a -> ( String, a )
        labelWithPrefix =
            (,) prefix

        labelNested : Select -> Maybe ( List ( String, OrderBy ), List ( String, Filter ), List ( String, Maybe Int ) )
        labelNested field =
            case field of
                Simple _ ->
                    Nothing

                Nested nestedName nestedParams ->
                    Just (labelParams' (prefix ++ nestedName ++ ".") nestedParams)

        appendTriples :
            ( appendable, appendable', appendable'' )
            -> ( appendable, appendable', appendable'' )
            -> ( appendable, appendable', appendable'' )
        appendTriples ( os1, fs1, ls1 ) ( os2, fs2, ls2 ) =
            ( os1 ++ os2, fs1 ++ fs2, ls1 ++ ls2 )

        labeledOrders : List ( String, OrderBy )
        labeledOrders =
            List.map labelWithPrefix params.order

        labeledFilters : List ( String, Filter )
        labeledFilters =
            List.map labelWithPrefix params.filter

        labeledLimit : List ( String, Maybe Int )
        labeledLimit =
            [ labelWithPrefix params.limit ]
    in
        params.select
            |> List.filterMap labelNested
            |> List.foldl appendTriples ( labeledOrders, labeledFilters, labeledLimit )


{-| NOTE: What if we were to label when we add?
OrderBy, Filter, and Limit (we would add a type) could have a (Maybe String)
which is populated with Nothing by default and changed to Just prefix whenever
a query is included in another query. We would still need an operation to flatten
the QueryParams, but the logic would be much simpler (would no longer be a weird
concatMap) This may be a good idea / improve performance a smudge (prematureoptimzation much?)
-}
labelParams : QueryParams -> ( List ( String, OrderBy ), List ( String, Filter ), List ( String, Maybe Int ) )
labelParams =
    labelParams' ""


{-| -}
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


{-| -}
labeledOrdersToKeyValue : List ( String, OrderBy ) -> List ( String, String )
labeledOrdersToKeyValue orders =
    let
        orderToString : OrderBy -> String
        orderToString order =
            case order of
                Asc name ->
                    name ++ ".asc"

                Desc name ->
                    name ++ ".desc"

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


{-| -}
labeledLimitsToKeyValue : List ( String, Maybe Int ) -> List ( String, String )
labeledLimitsToKeyValue limits =
    let
        toKeyValue : ( String, Maybe Int ) -> Maybe ( String, String )
        toKeyValue labeledLimit =
            case labeledLimit of
                ( _, Nothing ) ->
                    Nothing

                ( prefix, Just limit ) ->
                    Just ( "limit" ++ prefix, toString limit )
    in
        List.filterMap toKeyValue limits
