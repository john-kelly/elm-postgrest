module Rest.Adapters exposing (postgRest)

{-| Rest.Adapters
# Adapters
@docs postgRest
-}

import Rest.Types exposing (..)
import Http
import String


{-| -}
postgRest : RestRequest schema -> Http.Request
postgRest request =
    let
        { url, properties, filters, orders, limits, offset, singular, suppressCount, verb, resource } =
            unwrapRestRequest request

        ( resourceName, _ ) =
            unwrapResource resource

        trailingSlashUrl =
            if String.right 1 url == "/" then
                url
            else
                url ++ "/"

        requestUrl =
            [ ordersToKeyValue orders
            , propertiesToKeyValue properties
            , filtersToKeyValues filters
            , offsetToKeyValue offset
            , limitsToKeyValues resource limits
            ]
                |> List.foldl (++) []
                |> Http.url (trailingSlashUrl ++ resourceName)

        pluralityHeader =
            if singular then
                [ ( "Prefer", "plurality=singular" ) ]
            else
                []

        countHeader =
            if suppressCount then
                [ ( "Prefer", "count=none" ) ]
            else
                []

        headers =
            pluralityHeader ++ countHeader
    in
        { verb = verb
        , headers = headers
        , url = requestUrl
        , body = Http.empty
        }


propertiesToKeyValue : List Property -> List ( String, String )
propertiesToKeyValue properties =
    let
        propertyToString : Property -> String
        propertyToString property =
            case property of
                SimpleProperty name ->
                    name

                NestedResource name nestedProperties ->
                    name ++ "{" ++ propertiesToString nestedProperties ++ "}"

        propertiesToString : List Property -> String
        propertiesToString properties =
            case properties of
                [] ->
                    "*"

                _ ->
                    properties
                        |> List.map propertyToString
                        |> join ","
    in
        case properties of
            [] ->
                []

            _ ->
                [ ( "select", propertiesToString properties ) ]


filtersToKeyValues : List Filter -> List ( String, String )
filtersToKeyValues filters =
    let
        -- `Maybe` b/c we should not be able to filter on a NestedProperty
        condToKeyValue : Condition -> Maybe ( String, String )
        condToKeyValue cond =
            case cond of
                LikeFilter (SimpleProperty name) str ->
                    Just ( name, "like." ++ str )

                EqFilter (SimpleProperty name) str ->
                    Just ( name, "eq." ++ str )

                GteFilter (SimpleProperty name) str ->
                    Just ( name, "gte." ++ str )

                GtFilter (SimpleProperty name) str ->
                    Just ( name, "gt." ++ str )

                LteFilter (SimpleProperty name) str ->
                    Just ( name, "lte." ++ str )

                LtFilter (SimpleProperty name) str ->
                    Just ( name, "lt." ++ str )

                ILikeFilter (SimpleProperty name) str ->
                    Just ( name, "ilike." ++ str )

                InFilter (SimpleProperty name) list ->
                    Just ( name, "in." ++ join "," list )

                IsFilter (SimpleProperty name) str ->
                    Just ( name, "is." ++ str )

                ContainsFilter (SimpleProperty name) str ->
                    Just ( name, "@@." ++ str )

                _ ->
                    Nothing

        filterToKeyValue : Filter -> Maybe ( String, String )
        filterToKeyValue filter =
            case filter of
                Filter negated cond ->
                    if negated then
                        Maybe.map (\( key, value ) -> ( key, "not." ++ value ))
                            (condToKeyValue cond)
                    else
                        (condToKeyValue cond)
    in
        List.filterMap filterToKeyValue filters


ordersToKeyValue : List OrderBy -> List ( String, String )
ordersToKeyValue orders =
    let
        -- `Maybe` b/c we should not be able to filter on a NestedProperty
        orderToString : OrderBy -> Maybe String
        orderToString order =
            case order of
                Ascending (SimpleProperty name) ->
                    Just (name ++ ".asc")

                Descending (SimpleProperty name) ->
                    Just (name ++ ".desc")

                _ ->
                    Nothing

        ordersToString : List OrderBy -> String
        ordersToString order =
            orders
                |> List.filterMap orderToString
                |> join ","
    in
        case orders of
            [] ->
                []

            _ ->
                [ ( "order", ordersToString orders ) ]


offsetToKeyValue : Int -> List ( String, String )
offsetToKeyValue offset =
    [ ( "offset", toString offset ) ]


limitsToKeyValues : Resource schema1 -> List ( Resource schema2, Int ) -> List ( String, String )
limitsToKeyValues resource limits =
    let
        ( resourceName, _ ) =
            unwrapResource resource

        limitToKeyValue ( offsetResource, offset ) =
            let
                ( offsetResourceName, _ ) =
                    unwrapResource offsetResource
            in
                if resourceName == offsetResourceName then
                    Just ( "limit", toString offset )
                else
                    -- TODO This does not account for nested limits.
                    Nothing
    in
        List.filterMap limitToKeyValue limits



-- General Helpers


join : String -> List String -> String
join separator strings =
    strings
        |> List.indexedMap (,)
        |> List.foldl
            (\( i, next ) total ->
                total
                    ++ next
                    ++ if i /= (List.length strings) - 1 then
                        separator
                       else
                        ""
            )
            ""
