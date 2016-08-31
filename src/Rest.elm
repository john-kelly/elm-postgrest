module Rest
    exposing
        ( Resource
        , Property
        , resource
        , property
        , nested
        , RestRequest
        , read
        , send
        , select
        , Filter
        , filter
        , like
        , eq
        , gte
        , gt
        , lte
        , lt
        , neq
        , ilike
        , in'
        , notin
        , is
        , isnot
        , contains
        , not'
        , OrderBy
        , order
        , asc
        , desc
        , paginate
        , offset
        , limit
        , singular
        , count
        )

{-| DEPRECATED: Renamed to http://package.elm-lang.org/john-kelly/query
# Types
@docs Resource, Property, RestRequest, Filter, OrderBy
# Functions
@docs resource, property, nested, read, send, select, filter, like, eq, gte, gt, lte, lt, neq, ilike, in', notin, is, isnot, contains, not', order, asc, desc, paginate, offset, limit, singular, suppressCount
-}

import Http
import String
import Task
import Rest.Types as RT exposing (..)


{-| -}
type alias Resource schema =
    RT.Resource schema


{-| -}
type alias RestRequest schema =
    RT.RestRequest schema


{-| -}
type alias Property =
    RT.Property


{-| -}
type alias Filter =
    RT.Filter


{-| -}
type alias OrderBy =
    RT.OrderBy



-- Resource Builder


{-| -}
resource : String -> schema -> Resource schema
resource =
    Resource


{-| -}
property : String -> Property
property =
    SimpleProperty


{-| -}
nested : Resource schema1 -> List (schema1 -> Property) -> schema2 -> Property
nested resource propertyAccessors =
    let
        ( resourceName, resourceSchema ) =
            unwrapResource resource

        nestedProperty =
            propertyAccessors
                |> List.map (\fn -> fn resourceSchema)
                |> NestedResource resourceName
    in
        always nestedProperty



-- RestRequest Builder


{-| -}
read : String -> Resource schema -> RestRequest schema
read url resource =
    RestRequest
        { properties = []
        , filters = []
        , orders = []
        , limits = []
        , offset = 0
        , singular = False
        , suppressCount = True
        , verb = "GET"
        , resource = resource
        , url = url
        }



-- Selecting


{-| -}
select : List (schema -> Property) -> RestRequest schema -> RestRequest schema
select propertyAccessors request =
    let
        unwrappedRequest =
            unwrapRestRequest request

        ( _, resourceSchema ) =
            unwrapResource unwrappedRequest.resource
    in
        RestRequest
            { unwrappedRequest
              -- NOTE: we append new props, is this the best api?
                | properties = unwrappedRequest.properties ++ List.map (\fn -> fn resourceSchema) propertyAccessors
            }



-- Filtering
-- TODO: take a look here for api example: https://docs.djangoproject.com/en/1.10/ref/models/querysets/#field-lookups


{-| -}
filter : List (schema -> Filter) -> RestRequest schema -> RestRequest schema
filter filterAccessors request =
    let
        unwrappedRequest =
            unwrapRestRequest request

        ( _, resourceSchema ) =
            unwrapResource unwrappedRequest.resource
    in
        RestRequest
            { unwrappedRequest
                | filters = unwrappedRequest.filters ++ List.map (\fn -> fn resourceSchema) filterAccessors
            }


{-| -}
toFilterFn : (Property -> schema1 -> Condition) -> schema1 -> (schema2 -> Property) -> (schema2 -> Filter)
toFilterFn condValueConstructor val propertyAccessor =
    (\schema -> Filter False (condValueConstructor (propertyAccessor schema) val))


{-| -}
like : String -> (schema -> Property) -> (schema -> Filter)
like =
    toFilterFn LikeFilter


{-| -}
eq : String -> (schema -> Property) -> (schema -> Filter)
eq =
    toFilterFn EqFilter


{-| -}
gte : String -> (schema -> Property) -> (schema -> Filter)
gte =
    toFilterFn GteFilter


{-| -}
gt : String -> (schema -> Property) -> (schema -> Filter)
gt =
    toFilterFn GtFilter


{-| -}
lte : String -> (schema -> Property) -> (schema -> Filter)
lte =
    toFilterFn LteFilter


{-| -}
lt : String -> (schema -> Property) -> (schema -> Filter)
lt =
    toFilterFn LtFilter


{-| -}
neq : String -> (schema -> Property) -> (schema -> Filter)
neq =
    -- TODO: DEPRECATE in favor of smaller base api.
    not' eq


{-| -}
ilike : String -> (schema -> Property) -> (schema -> Filter)
ilike =
    -- TODO: What is the best name for this? Too low level?
    toFilterFn ILikeFilter


{-| -}
in' : List String -> (schema -> Property) -> (schema -> Filter)
in' =
    -- TODO: What is the best name for this?
    toFilterFn InFilter


{-| -}
notin : List String -> (schema -> Property) -> (schema -> Filter)
notin =
    -- TODO: DEPRECATE
    not' in'


{-| -}
is : String -> (schema -> Property) -> (schema -> Filter)
is =
    toFilterFn IsFilter


{-| -}
isnot : String -> (schema -> Property) -> (schema -> Filter)
isnot =
    -- TODO: DEPRECATE
    not' is


{-| -}
contains : String -> (schema -> Property) -> (schema -> Filter)
contains =
    -- TODO: Is this the right name? I don't think so.
    -- https://docs.djangoproject.com/en/1.10/ref/models/querysets/#contains
    toFilterFn ContainsFilter


{-| -}
not' : (a -> (schema -> Property) -> (schema -> Filter)) -> a -> (schema -> Property) -> (schema -> Filter)
not' filterAccessorConstructor val propertyAccessor =
    -- TODO: What is the best name for this?
    let
        filterAccessor =
            filterAccessorConstructor val propertyAccessor
    in
        (\schema ->
            case filterAccessor schema of
                Filter negated cond ->
                    Filter (not negated) cond
        )



-- Ordering


{-| -}
order : List (schema -> OrderBy) -> RestRequest schema -> RestRequest schema
order orderByAccessors request =
    let
        unwrappedRequest =
            unwrapRestRequest request

        ( _, schema ) =
            unwrapResource unwrappedRequest.resource
    in
        RestRequest
            { unwrappedRequest
                | orders = unwrappedRequest.orders ++ List.map (\fn -> fn schema) orderByAccessors
            }


{-| -}
asc : (schema -> Property) -> (schema -> OrderBy)
asc propertyAccessor =
    (\schema -> Ascending (propertyAccessor schema))


{-| -}
desc : (schema -> Property) -> (schema -> OrderBy)
desc propertyAccessor =
    (\schema -> Descending (propertyAccessor schema))



-- Count and Pagination


{-| -}
offset : Int -> RestRequest schema -> RestRequest schema
offset offset' request =
    -- TODO: setting?
    let
        unwrapped =
            unwrapRestRequest request
    in
        RestRequest { unwrapped | offset = offset' }


{-| -}
limit : List ( Resource schema, Int ) -> RestRequest schema -> RestRequest schema
limit limits request =
    let
        unwrapped =
            unwrapRestRequest request
    in
        RestRequest { unwrapped | limits = unwrapped.limits ++ limits }


{-| -}
paginate : Int -> Int -> RestRequest schema -> RestRequest schema
paginate pageSize pageNumber request =
    let
        unwrapped =
            unwrapRestRequest request
    in
        RestRequest
            { unwrapped
              -- TODO: should this append?
                | limits = [ ( unwrapped.resource, pageSize ) ]
                , offset = (pageNumber - 1) * pageSize
            }


{-| -}
singular : RestRequest schema -> RestRequest schema
singular request =
    let
        unwrapped =
            unwrapRestRequest request
    in
        RestRequest { unwrapped | singular = True }


{-| -}
count : RestRequest schema -> RestRequest schema
count request =
    -- NOTE: maybe this belongs as a settings? it's nice to have it as a fn,
    -- but it allows for a potentially confusing user interaction of calling
    -- count more than once. what other functions may belong as settings?
    -- this might be a perfect canidate for a Rest.Settings! the adapter can go
    -- in there too, and dev mode options, etc.
    let
        unwrapped =
            unwrapRestRequest request
    in
        RestRequest { unwrapped | suppressCount = False }



-- RestRequest Task Builder


{-| -}
send : (RestRequest schema -> Http.Request) -> Http.Settings -> RestRequest schema -> Task.Task Http.RawError Http.Response
send adapter settings restRequest =
    restRequest
        |> adapter
        |> Http.send settings
