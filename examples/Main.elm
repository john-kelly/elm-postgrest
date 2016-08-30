module Main exposing (..)

-- Core

import Html exposing (Html, text)
import Html.App as App
import Http
import Task
import Rest exposing (..)
import Rest.Adapters exposing (postgRest)


session =
    resource "sessions"
        { id = property "id"
        , speaker_id = property "speaker_id"
        , start_time = property "start_time"
        , end_time = property "end_time"
        , location = property "location"
        , session_type = property "session_type"
        }


speaker =
    resource "speakers"
        { id = property "id"
        , name = property "name"
        , lineup_order = property "lineup_order"
        , twitter = property "twitter"
        , avatar_url = property "avatar_url"
        , bio = property "bio"
        , featured = property "featured"
        }


sessionsCmd =
    read "http://postgrest.herokuapp.com/" session
        |> select
            [ .id
            , .start_time
            , .location
            , nested speaker []
            ]
        |> filter
            [ .location |> like "%Russia%"
            , .session_type |> eq "workshop"
            ]
        |> order [ asc .start_time ]
        |> send postgRest Http.defaultSettings
        |> Task.perform FetchFail FetchSucceed


main =
    App.program
        { init = ( { sessions = Nothing }, sessionsCmd )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { sessions : Maybe Http.Response
    }



-- UPDATE


type Msg
    = FetchSucceed Http.Response
    | FetchFail Http.RawError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSucceed sessions ->
            ( { model | sessions = Just sessions }, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { sessions } =
    toString sessions |> text
