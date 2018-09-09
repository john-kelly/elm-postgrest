module ReadMeExample exposing (main)

import Browser
import Html exposing (Html, div, text)
import Http
import PostgRest as Rest
import Schema


type alias School =
    { id : String
    , name : String
    }


request : Rest.Request (List School)
request =
    Rest.readMany Schema.school
        { select = selection
        , where_ = Rest.eq "CA" .state
        , order = [ Rest.asc .name ]
        , limit = Nothing
        , offset = Nothing
        }


selection : Rest.Selection { a | id : Rest.Attribute String, name : Rest.Attribute String } School
selection =
    Rest.map2 School
        (Rest.field .id)
        (Rest.field .name)


type alias Model =
    List School


type Msg
    = Fetch (Result Http.Error (List School))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch (Ok schools) ->
            ( schools, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] (List.map (\school -> div [] [ text school.id, text ": ", text school.name ]) model)


page : Model -> { title : String, body : List (Html Msg) }
page model =
    { title = "README Example"
    , body = [ view model ]
    }


getSchools : Cmd Msg
getSchools =
    Http.send Fetch (Rest.toHttpRequest { timeout = Nothing, token = Nothing, url = "http://localhost:3000" } request)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( [], getSchools )
        , view = page
        , update = update
        , subscriptions = \_ -> Sub.none
        }
