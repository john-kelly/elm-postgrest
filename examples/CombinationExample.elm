module CombinationExample exposing (..)

import Browser
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src, style)
import Http
import PostgRest as Rest
    exposing
        ( Attribute
        , Request
        , Schema
        , Selection
        )


getPokemon : Cmd Msg
getPokemon =
    Rest.readAll pokemonSchema pokemonSelection
        |> Rest.toHttpRequest
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
    Rest.map3 Pokemon
        (Rest.field .image)
        (Rest.field .name)
        (Rest.field .id)


pokemonSchema :
    Schema x
        { id : Attribute Int
        , name : Attribute String
        , image : Attribute String
        }
pokemonSchema =
    Rest.schema "pokemons"
        { id = Rest.int "id"
        , name = Rest.string "name"
        , image = Rest.string "image"
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
    Browser.document
        { init = \_ -> ( [], getPokemon )
        , view = page
        , update = update
        , subscriptions = \_ -> Sub.none
        }
