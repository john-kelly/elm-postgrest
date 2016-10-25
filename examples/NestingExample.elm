module Main exposing (..)

import Html
import Http
import PostgRest as PG
import Resources


type alias Session =
    { id : Int
    , location : String
    , start_time : String
    , speaker : Speaker
    }


type alias Speaker =
    { id : Int
    , name : String
    }


sessionCmd =
    let
        speakerQuery =
            PG.query Resources.speaker Speaker
                |> PG.select .id
                |> PG.select .name
    in
        PG.query Resources.session Session
            |> PG.select .id
            |> PG.select .location
            |> PG.select .start_time
            |> PG.include speakerQuery
            |> PG.filter [ .location |> PG.not PG.ilike "%russia%" ]
            |> PG.order [ PG.asc .start_time ]
            |> PG.list Nothing "http://postgrest.herokuapp.com/"
            |> Http.send Fetch


main =
    Html.program
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
    = Fetch (Result Http.Error (List Session))


update msg model =
    case msg of
        Fetch (Ok sessions) ->
            ( { model | sessions = sessions }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view { sessions } =
    toString sessions |> Html.text
