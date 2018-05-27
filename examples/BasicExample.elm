module BasicExample exposing (..)

import Browser
import Html exposing (Html, div, img)
import Html.Attributes exposing (src)
import Http
import PostgRest as PG
    exposing
        ( Attribute
        , Request
        , Schema
        , Selection
        )


getPokemon : Cmd Msg
getPokemon =
    PG.readAll pokemonSchema pokemonSelection
        |> PG.toHttpRequest
            { timeout = Nothing
            , token = Nothing
            , url = "http://localhost:3000"
            }
        |> Http.send Fetch


pokemonSelection :
    Selection
        { attributes
            | image : Attribute String
        }
        String
pokemonSelection =
    PG.field .image


pokemonSchema :
    Schema x
        { id : Attribute Int
        , name : Attribute String
        , image : Attribute String
        }
pokemonSchema =
    PG.schema "pokemons"
        { id = PG.int "id"
        , name = PG.string "name"
        , image = PG.string "image"
        }


type alias Model =
    List String


type Msg
    = Fetch (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch (Ok images) ->
            ( images, Cmd.none )

        _ ->
            ( model, Cmd.none )


viewPokemon : String -> Html Msg
viewPokemon url =
    img [ src url ] []


view : Model -> Html Msg
view model =
    div [] (List.map viewPokemon model)


page : Model -> { title : String, body : List (Html Msg) }
page model =
    { title = "Basic Example"
    , body = [ view model ]
    }


main : Program () Model Msg
main =
    Browser.fullscreen
        { init = \_ -> ( [], getPokemon )
        , view = page
        , update = update
        , onNavigation = Nothing
        , subscriptions = \_ -> Sub.none
        }
