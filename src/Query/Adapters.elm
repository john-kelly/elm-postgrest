module Query.Adapters exposing (postgRest)

{-| -}

import String
import Http
import Dict
import Query.Types exposing (..)
import Query exposing (Settings)


{-| -}
postgRest : String -> Settings -> Query s -> Http.Request
postgRest url settings query =
    let
        { count, singular, limit, offset } =
            settings

        ( name, _, params ) =
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
            , limitToKeyValues limit
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


fieldsToKeyValue : List Field -> List ( String, String )
fieldsToKeyValue fields =
    let
        fieldToString : Field -> String
        fieldToString field =
            case field of
                Simple name ->
                    name

                Nested name { select } ->
                    name ++ "{" ++ fieldsToString select ++ "}"

        fieldsToString : List Field -> String
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

        filterToKeyValue : ( String, Filter ) -> Maybe ( String, String )
        filterToKeyValue ( prefix, filter ) =
            case filter of
                Filter True cond (Simple key) ->
                    Just ( prefix ++ key, "not." ++ contToString cond )

                Filter False cond (Simple key) ->
                    Just ( prefix ++ key, contToString cond )

                Filter _ _ (Nested _ _) ->
                    Nothing
    in
        List.filterMap filterToKeyValue filters


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
                            |> List.filterMap orderToString
                            |> String.join ","
                        )

        orderToString : OrderBy -> Maybe String
        orderToString order =
            case order of
                Asc (Simple name) ->
                    Just (name ++ ".asc")

                Desc (Simple name) ->
                    Just (name ++ ".desc")

                _ ->
                    Nothing
    in
        orders
            |> List.foldr
                (\( prefix, order ) dict ->
                    Dict.update prefix
                        (\m ->
                            case m of
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
