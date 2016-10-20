module Main exposing (..)

import Html exposing (Html, text)
import Html.App as App
import Http
import Task
import PostgRest exposing (..)
import Json.Decode as Decode
import Debug


session =
    resource "sessions"
        { id = int "id"
        , speaker_id = int "speaker_id"
        , start_time = string "start_time"
        , end_time = string "end_time"
        , location = string "location"
        , session_type = int "session_type"
        }


type alias Session =
    { id : Int
    , location : String
    , start_time : String
    }


sessionCmd =
    query session Session
        |> select .id
        |> select .location
        |> select .start_time
        |> filter [ .location |> not' ilike "%russia%" ]
        |> order [ asc .start_time ]
        |> list Nothing "http://postgrest.herokuapp.com/"
        |> Task.perform FetchFail FetchSucceed


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
            let
                _ =
                    Debug.log "error" a
            in
                ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { sessions } =
    toString sessions |> text
