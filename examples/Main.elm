module Main exposing (..)

import Html exposing (Html, text)
import Html.App as App
import Http
import Task
import PostgRest exposing (..)
import Json.Decode as Decode
import Debug


session =
    schema "sessions"
        { id = field "id" Decode.int
        , speaker_id = field "speaker_id" Decode.int
        , start_time = field "start_time" Decode.int
        , end_time = field "end_time" Decode.int
        , location = field "location" Decode.string
        , session_type = field "session_type" Decode.int
        }


speaker =
    schema "speakers"
        { id = field "id" Decode.int
        , name = field "name" Decode.string
        , lineup_order = field "lineup_order" Decode.int
        , twitter = field "twitter" Decode.int
        , avatar_url = field "avatar_url" Decode.int
        , bio = field "bio" Decode.int
        , featured = field "featured" Decode.int
        }


type alias Session =
    { id : Int
    , location : String
    , speakers : Speaker
    }


type alias Speaker =
    { id : Int
    , name : String
    }


speakerQuery =
    query speaker Speaker
        |> select .id
        |> select .name


sessionCmd =
    query session Session
        |> select .id
        |> select .location
        |> include speakerQuery
        |> filter [ .location |> like "%Russia%" ]
        |> order [ asc .id, desc .location ]
        |> list "http://postgrest.herokuapp.com/" PostgRest.defaultSettings
        |> Task.perform FetchFail FetchSucceed



-- ((select .id) >> (select .location) >> (include speakerQuery))


main =
    App.program
        { init = ( { sessions = [] }, sessionCmd )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { sessions : List Session
    }



-- UPDATE


type Msg
    = FetchSucceed (List Session)
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSucceed sessions ->
            ( { model | sessions = sessions }, Cmd.none )

        FetchFail a ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { sessions } =
    toString sessions |> text
