module CombinationExample exposing (..)

import Browser
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src, style)
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


type alias Pokemon =
    { image : String
    , name : String
    , id : Int
    }


pokemonSelection :
    Selection
        { attributes
            | image : Attribute String
            , name : Attribute String
            , id : Attribute Int
        }
        Pokemon
pokemonSelection =
    PG.map3 Pokemon
        (PG.field .image)
        (PG.field .name)
        (PG.field .id)


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
    List Pokemon


type Msg
    = Fetch (Result Http.Error (List Pokemon))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch (Ok pokemon) ->
            ( pokemon, Cmd.none )

        _ ->
            ( model, Cmd.none )


viewPokemon : Pokemon -> Html Msg
viewPokemon pokemon =
    div [ style "display" "inline-block" ]
        [ img [ src pokemon.image ] []
        , div [ style "text-align" "center" ] [ text ("#" ++ String.fromInt pokemon.id ++ ": " ++ pokemon.name) ]
        ]


view : Model -> Html Msg
view model =
    div [] (List.map viewPokemon model)


page : Model -> { title : String, body : List (Html Msg) }
page model =
    { title = "Combination Example"
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
