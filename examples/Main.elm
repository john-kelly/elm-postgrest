module Main exposing (..)

-- Core

import Html exposing (Html, text)
import Html.App as App
import Http
import Task
import Query exposing (..)


session =
    schema "sessions"
        { id = field "id"
        , speaker_id = field "speaker_id"
        , start_time = field "start_time"
        , end_time = field "end_time"
        , location = field "location"
        , session_type = field "session_type"
        }



-- List


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


speakerQuery =
    query speaker
        |> select [ .id, .bio ]
        |> filter [ .id |> gte 10 ]
        |> order [ asc .name ]


sessionQuery =
    query session
        |> select
            [ .id
            , .speaker_id
            , .start_time
            , (.) speakerQuery
            ]
        |> filter [ .location |> not' like "%Russia%" ]
        |> order [ asc .start_time ]
        |> postgRest "http://postgrest.herokuapp.com/" defaultSettings


main =
    App.program
        { init = ( { sessions = Nothing }, Cmd.none )
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
    toString sessionQuery |> text
