module Main exposing (..)

import Html
import Http
import PostgRest as PG
import Resources


type alias Session =
    { id : Int
    , location : String
    , start_time : String
    }


sessionCmd =
    PG.query Resources.session Session
        |> PG.select .id
        |> PG.select .location
        |> PG.select .start_time
        |> PG.paginate { pageSize = 2, pageNumber = 1 } "http://postgrest.herokuapp.com/"
        |> Http.send Fetch


main =
    Html.program
        { init = ( { sessions = PG.Page [] 0 }, sessionCmd )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { sessions : PG.Page Session
    }



-- UPDATE


type Msg
    = Fetch (Result Http.Error (PG.Page Session))


update msg model =
    case msg of
        Fetch (Ok sessions) ->
            ( { model | sessions = sessions }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view { sessions } =
    toString sessions |> Html.text
