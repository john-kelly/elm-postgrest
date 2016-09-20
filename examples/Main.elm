module Main exposing (..)

import Html exposing (Html, text)
import Html.App as App
import Http
import Task
import PostgRest exposing (..)
import Json.Decode as Decode
import Debug


study =
    resource "study"
        { id = field "id" Decode.int
        , title = field "title" Decode.string
        , description = field "description" Decode.string
        , assist_id = field "assist_id" Decode.string
        }


discipline =
    resource "discipline"
        { id = field "id" Decode.int
        , title = field "title" Decode.string
        }


type alias Study =
    { id : Int
    , title : String
    , description : String
    , assist_id : String
    }


type alias Discipline =
    { id : Int
    , title : String
    }


studyCmd =
    query study Study
        |> select .id
        |> select .title
        |> select .description
        |> select .assist_id
        |> filter [ .title |> like "P%" ]
        |> list (Just 1) "http://localhost:3000/"
        |> Task.perform FetchFail FetchSucceed


main =
    App.program
        { init = ( { studies = [] }, studyCmd )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { studies : List Study
    }



-- UPDATE


type Msg
    = FetchSucceed (List Study)
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSucceed studies ->
            ( { model | studies = studies }, Cmd.none )

        FetchFail a ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { studies } =
    toString studies |> text
