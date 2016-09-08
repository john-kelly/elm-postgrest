module Main exposing (..)

import Html exposing (Html, text)
import Html.App as App
import Http
import Task
import Query exposing (..)
import Query.Adapters exposing (postgRest)
import Query.Infix exposing ((.))


session =
    schema "sessions"
        { id = field "id"
        , speaker_id = field "speaker_id"
        , start_time = field "start_time"
        , end_time = field "end_time"
        , location = field "location"
        , session_type = field "session_type"
        }


speaker =
    schema "speakers"
        { id = field "id"
        , name = field "name"
        , lineup_order = field "lineup_order"
        , twitter = field "twitter"
        , avatar_url = field "avatar_url"
        , bio = field "bio"
        , featured = field "featured"
        }


sessionCmd =
    let
        speakerQuery =
            query speaker
                |> select [ .id, .bio ]
    in
        query session
            |> select
                [ .id
                , .speaker_id
                , .start_time
                , .location
                , (.) speakerQuery
                ]
            |> filter [ .location |> not' like "%Russia%" ]
            |> order [ desc .id ]
            |> postgRest "http://postgrest.herokuapp.com/" Query.defaultSettings
            |> Http.send Http.defaultSettings
            |> Task.perform FetchFail FetchSucceed


main =
    App.program
        { init = ( { sessions = Nothing }, sessionCmd )
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
