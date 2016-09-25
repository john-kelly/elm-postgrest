module PostgRest.Http exposing (fromPage)

{-| Internal Helper Module for Http
-}

import Dict
import Http
import Json.Decode as Decode
import Regex
import String
import Task


{-| -}
type alias Page a =
    { data : List a
    , count : Int
    }


{-| inspiredBy from evancz/elm-http
-}
fromPage : Decode.Decoder (List a) -> Task.Task Http.RawError Http.Response -> Task.Task Http.Error (Page a)
fromPage decoder response =
    let
        decode str count =
            case Decode.decodeString decoder str of
                Result.Ok v ->
                    Task.succeed { data = v, count = count }

                Result.Err msg ->
                    Task.fail (Http.UnexpectedPayload msg)
    in
        Task.mapError promoteError response
            `Task.andThen` handleResponse decode


{-| copied from evancz/elm-http
-}
promoteError : Http.RawError -> Http.Error
promoteError rawError =
    case rawError of
        Http.RawTimeout ->
            Http.Timeout

        Http.RawNetworkError ->
            Http.NetworkError


{-| inspiredBy from evancz/elm-http
-}
handleResponse : (String -> Int -> Task.Task Http.Error (Page a)) -> Http.Response -> Task.Task Http.Error (Page a)
handleResponse handle response =
    let
        getCount =
            Dict.get "Content-Range" response.headers
                `Maybe.andThen` (Regex.replace (Regex.All) (Regex.regex ".+\\/") (always "")
                                    >> String.toInt
                                    >> Result.toMaybe
                                )
    in
        if 200 <= response.status && response.status < 300 then
            case ( response.value, getCount ) of
                ( Http.Text str, Just contentCount ) ->
                    handle str contentCount

                _ ->
                    Task.fail (Http.UnexpectedPayload "Unexpected Payload")
        else
            Task.fail (Http.BadResponse response.status response.statusText)
