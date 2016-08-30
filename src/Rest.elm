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
        , suppressCount
        )

{-| Rest
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
type alias Resource a =
    RT.Resource a


{-| -}
type alias RestRequest a =
    RT.RestRequest a


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
resource : String -> a -> Resource a
resource =
    Resource


{-| -}
property : String -> Property
property =
    SimpleProperty


{-| -}
nested : Resource a -> List (a -> Property) -> b -> Property
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
read : String -> Resource a -> RestRequest a
read url resource =
    RestRequest
        { properties = []
        , filters = []
        , orders = []
        , limits = []
        , offset = 0
        , singular = False
        , suppressCount = False
        , verb = "GET"
        , resource = resource
        , url = url
        }



-- Selecting


{-| -}
select : List (a -> Property) -> RestRequest a -> RestRequest a
select propertyAccessors request =
    let
        unwrappedRequest =
            unwrapRestRequest request

        ( _, resourceSchema ) =
            unwrapResource unwrappedRequest.resource
    in
        RestRequest
            { unwrappedRequest
              -- TODO: what is the better api? adding or replacing?
              -- request.properties ++ List.map (\fn -> fn request.resource.schema) propertyAccessors
                | properties = List.map (\fn -> fn resourceSchema) propertyAccessors
            }



-- Filtering


{-| -}
filter : List (a -> Filter) -> RestRequest a -> RestRequest a
filter filterAccessors request =
    let
        unwrappedRequest =
            unwrapRestRequest request

        ( _, resourceSchema ) =
            unwrapResource unwrappedRequest.resource
    in
        RestRequest
            { unwrappedRequest
                | filters = List.map (\fn -> fn resourceSchema) filterAccessors
            }


{-| -}
toFilterFn : (Property -> a -> Condition) -> a -> (b -> Property) -> (b -> Filter)
toFilterFn condValueConstructor val propertyAccessor =
    (\schema -> Filter False (condValueConstructor (propertyAccessor schema) val))


{-| -}
like : String -> (a -> Property) -> (a -> Filter)
like =
    toFilterFn LikeFilter


{-| -}
eq : String -> (a -> Property) -> (a -> Filter)
eq =
    toFilterFn EqFilter


{-| -}
gte : String -> (a -> Property) -> (a -> Filter)
gte =
    toFilterFn GteFilter


{-| -}
gt : String -> (a -> Property) -> (a -> Filter)
gt =
    toFilterFn GtFilter


{-| -}
lte : String -> (a -> Property) -> (a -> Filter)
lte =
    toFilterFn LteFilter


{-| -}
lt : String -> (a -> Property) -> (a -> Filter)
lt =
    toFilterFn LtFilter


{-| -}
neq : String -> (a -> Property) -> (a -> Filter)
neq =
    not' eq


{-| -}
ilike : String -> (a -> Property) -> (a -> Filter)
ilike =
    toFilterFn ILikeFilter


{-| -}
in' : List String -> (a -> Property) -> (a -> Filter)
in' =
    toFilterFn InFilter


{-| -}
notin : List String -> (a -> Property) -> (a -> Filter)
notin =
    not' in'


{-| -}
is : String -> (a -> Property) -> (a -> Filter)
is =
    toFilterFn IsFilter


{-| -}
isnot : String -> (a -> Property) -> (a -> Filter)
isnot =
    not' is


{-| -}
contains : String -> (a -> Property) -> (a -> Filter)
contains =
    toFilterFn ContainsFilter


{-| -}
not' : (a -> (b -> Property) -> (b -> Filter)) -> a -> (b -> Property) -> (b -> Filter)
not' filterAccessorConstructor val propertyAccessor =
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
order : List (a -> OrderBy) -> RestRequest a -> RestRequest a
order orderByAccessors request =
    let
        unwrappedRequest =
            unwrapRestRequest request

        ( _, resourceSchema ) =
            unwrapResource unwrappedRequest.resource
    in
        RestRequest
            { unwrappedRequest
                | orders = List.map (\fn -> fn resourceSchema) orderByAccessors
            }


{-| -}
asc : (a -> Property) -> (a -> OrderBy)
asc propertyAccessor =
    (\schema -> Ascending (propertyAccessor schema))


{-| -}
desc : (a -> Property) -> (a -> OrderBy)
desc propertyAccessor =
    (\schema -> Descending (propertyAccessor schema))



-- Count and Pagination


{-| -}
offset : Int -> RestRequest a -> RestRequest a
offset offset' request =
    let
        unwrapped =
            unwrapRestRequest request
    in
        RestRequest { unwrapped | offset = offset' }


{-| -}
limit : List ( Resource a, Int ) -> RestRequest a -> RestRequest a
limit limits request =
    let
        unwrapped =
            unwrapRestRequest request
    in
        RestRequest { unwrapped | limits = limits }


{-| -}
paginate : Int -> Int -> RestRequest a -> RestRequest a
paginate pageSize pageNumber request =
    let
        unwrapped =
            unwrapRestRequest request
    in
        RestRequest
            { unwrapped
                | limits = [ ( unwrapped.resource, pageSize ) ]
                , offset = (pageNumber - 1) * pageSize
            }


{-| -}
singular : RestRequest a -> RestRequest a
singular request =
    let
        unwrapped =
            unwrapRestRequest request
    in
        RestRequest { unwrapped | singular = True }


{-| -}
suppressCount : RestRequest a -> RestRequest a
suppressCount request =
    let
        unwrapped =
            unwrapRestRequest request
    in
        RestRequest { unwrapped | suppressCount = True }



-- RestRequest Task Builder


{-| -}
send : (RestRequest a -> Http.Request) -> Http.Settings -> RestRequest a -> Task.Task Http.RawError Http.Response
send adapter settings restRequest =
    restRequest
        |> adapter
        |> Http.send settings
