module BasicExample exposing (..)

import PostgRest as PG
    exposing
        ( Attribute
        , Request
        , Schema
        , Selection
        )
import Html exposing (Html, img, div)
import Html.Attributes exposing (src)
import Http


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


view : Model -> Html Msg
view model =
    div [] (List.map viewPokemon model)


viewPokemon : String -> Html Msg
viewPokemon url =
    img [ src url ] []


main : Program Never Model Msg
main =
    Html.program
        { init = ( [], getPokemon )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
