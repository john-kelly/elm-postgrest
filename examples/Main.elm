module Main exposing (..)

-- Core

import Html exposing (Html, text)
import Html.App as App
import Http
import Task
import Rest exposing (..)
import Rest.Adapters exposing (postgRest)


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


speakersCmd =
    read "http://postgrest.herokuapp.com/" speaker
        |> select [ .id, .name ]
        |> order [ asc .name ]
        |> send postgRest Http.defaultSettings
        |> Task.perform FetchFail FetchSucceed


main =
    App.program
        { init = ( { speakers = Nothing }, speakersCmd )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { speakers : Maybe Http.Response
    }



-- UPDATE


type Msg
    = FetchSucceed Http.Response
    | FetchFail Http.RawError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSucceed speakers ->
            ( { model | speakers = Just speakers }, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { speakers } =
    toString speakers |> text
