module PostgRest
    exposing
        ( Field
        , Resource
        , Query
        , OrderBy
        , Filter
        , Page
        , field
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
        , first
        , paginate
        )

{-| A query builder library for PostgREST.

I recommend looking at the [examples](https://github.com/john-kelly/elm-postgrest/blob/master/examples/Main.elm) before diving into the API or source code.

# Define a Resource
@docs Resource, resource, Field, field

# Build a Query
@docs Query, query

### Selecting and Nesting
@docs select, include, includeMany

### Filtering
@docs Filter, filter, like, ilike, eq, gte, gt, lte, lt, in', is, not'

### Ordering
@docs OrderBy, order, asc, desc

# Send a Query
@docs list, first

### Pagination
@docs Page, paginate

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
type Resource schema
    = Resource String schema


{-| -}
type Query schema a
    = Query String schema QueryParams (Decode.Decoder a)


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
resource : String -> schema -> Resource schema
resource =
    Resource


{-| -}
field : String -> Decode.Decoder a -> Field a
field =
    Field


{-| -}
query : Resource schema -> (a -> b) -> Query schema (a -> b)
query (Resource name schema) recordCtor =
    Query name
        schema
        { select = [], filter = [], order = [], limit = Nothing }
        (Decode.succeed recordCtor)


{-| -}
include : Query schema2 a -> Query schema1 (a -> b) -> Query schema1 b
include (Query subName subShape subParams subDecoder) (Query queryName queryShape queryParams queryDecoder) =
    Query queryName
        queryShape
        { queryParams | select = Nested subName subParams :: queryParams.select }
        (apply queryDecoder (subName := subDecoder))


{-| -}
includeMany : Maybe Int -> Query schema2 a -> Query schema1 (List a -> b) -> Query schema1 b
includeMany limit (Query subName subShape subParams subDecoder) (Query queryName queryShape queryParams queryDecoder) =
    Query queryName
        queryShape
        { queryParams | select = Nested subName { subParams | limit = limit } :: queryParams.select }
        (apply queryDecoder (subName := Decode.list subDecoder))


{-| -}
select : (schema -> Field a) -> Query schema (a -> b) -> Query schema b
select fieldAccessor (Query name schema params decoder) =
    case fieldAccessor schema of
        Field fieldName fieldDecoder ->
            Query name
                schema
                { params | select = Simple fieldName :: params.select }
                (apply decoder (fieldName := fieldDecoder))


{-| -}
order : List (schema -> OrderBy) -> Query schema a -> Query schema a
order orders (Query name schema params decoder) =
    Query name
        schema
        { params | order = params.order ++ List.map (\fn -> fn schema) orders }
        decoder


{-| Apply filters to a query
-}
filter : List (schema -> Filter) -> Query schema a -> Query schema a
filter filters (Query name schema params decoder) =
    Query name
        schema
        { params | filter = params.filter ++ List.map (\fn -> fn schema) filters }
        decoder


{-| -}
singleValueFilterFn : (String -> Condition) -> a -> (schema -> Field b) -> schema -> Filter
singleValueFilterFn condCtor condArg attrAccessor schema =
    case attrAccessor schema of
        Field name _ ->
            Filter False (condCtor (coerceToString condArg)) name


{-|
Simple [pattern matching](https://www.postgresql.org/docs/9.5/static/functions-matching.html#FUNCTIONS-LIKE)
-}
like : String -> (schema -> Field String) -> schema -> Filter
like =
    singleValueFilterFn Like


{-| Case-insensitive `like`
-}
ilike : String -> (schema -> Field String) -> schema -> Filter
ilike =
    singleValueFilterFn ILike


{-| Equals
-}
eq : a -> (schema -> Field a) -> schema -> Filter
eq =
    singleValueFilterFn Eq


{-| Greater than or equal to
-}
gte : a -> (schema -> Field a) -> schema -> Filter
gte =
    singleValueFilterFn Gte


{-| Greater than
-}
gt : a -> (schema -> Field a) -> schema -> Filter
gt =
    singleValueFilterFn Gt


{-| Less than or equal to
-}
lte : a -> (schema -> Field a) -> schema -> Filter
lte =
    singleValueFilterFn Lte


{-| Less than
-}
lt : a -> (schema -> Field a) -> schema -> Filter
lt =
    singleValueFilterFn Lt


{-| In List
-}
in' : List a -> (schema -> Field a) -> schema -> Filter
in' condArgs attrAccessor schema =
    case attrAccessor schema of
        Field name _ ->
            Filter False (In (List.map coerceToString condArgs)) name


{-| Is comparison
-}
is : a -> (schema -> Field a) -> schema -> Filter
is =
    singleValueFilterFn Is


{-| Negate a Filter
-}
not' : (a -> (schema -> Field a) -> (schema -> Filter)) -> a -> (schema -> Field a) -> schema -> Filter
not' filterAccessorCtor val fieldAccessor schema =
    case filterAccessorCtor val fieldAccessor schema of
        Filter negated cond fieldName ->
            Filter (not negated) cond fieldName


{-| Ascending
-}
asc : (schema -> Field a) -> schema -> OrderBy
asc fieldAccessor schema =
    case fieldAccessor schema of
        Field name _ ->
            Asc name


{-| Descending
-}
desc : (schema -> Field a) -> schema -> OrderBy
desc fieldAccessor schema =
    case fieldAccessor schema of
        Field name _ ->
            Desc name



-- naming for these functions is based off of:
-- http://www.django-rest-framework.org/api-guide/generic-views/#retrieveupdateapiview


{-| Takes `limit`, `url` and a `query`, returning a list of objects from database on success and Http.Error otherwise
-}
list : Maybe Int -> String -> Query schema a -> Task.Task Http.Error (List a)
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
first : String -> Query schema a -> Task.Task Http.Error a
first url (Query name _ params decoder) =
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
paginate : { pageNumber : Int, pageSize : Int } -> String -> Query schema a -> Task.Task Http.Error (Page a)
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
