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
    }


type alias Speaker =
    { id : Int
    , name : String
    , sessions : List Session
    }


sessionQuery =
    query session Session
        |> select .id


speakerCmd =
    query speaker Speaker
        |> select .id
        |> select .name
        |> includeMany Nothing sessionQuery
        |> list Nothing "http://postgrest.herokuapp.com/"
        |> Task.perform FetchFail FetchSucceed



-- ((select .id) >> (select .location) >> (include speakerQuery))


main =
    App.program
        { init = ( { speakers = [] }, speakerCmd )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { speakers : List Speaker
    }



-- UPDATE


type Msg
    = FetchSucceed (List Speaker)
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSucceed speakers ->
            ( { model | speakers = speakers }, Cmd.none )

        FetchFail a ->
            let
                _ =
                    Debug.log "hell" a
            in
                ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { speakers } =
    toString speakers |> text
