module RelationshipExample exposing (..)

import Browser
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src, style)
import Http
import PostgRest as Rest
    exposing
        ( Attribute
        , HasMany
        , Relationship
        , Request
        , Schema
        , Selection
        )


getTrainers : Cmd Msg
getTrainers =
    Rest.readAll trainerSchema trainerSelection
        |> Rest.toHttpRequest
            { timeout = Nothing
            , token = Nothing
            , url = "http://localhost:3000"
            }
        |> Http.send Fetch


type alias Trainer =
    { name : String
    , image : String
    , pokemon : List Pokemon
    }


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


trainerSelection :
    Selection
        { attributes
            | image : Attribute String
            , name : Attribute String
            , pokemon : Relationship HasMany PokemonSchema
        }
        Trainer
trainerSelection =
    Rest.map3 Trainer
        (Rest.field .name)
        (Rest.field .image)
        (Rest.embedAll .pokemon pokemonSchema pokemonSelection)


type PokemonSchema
    = PokemonSchema PokemonSchema


pokemonSchema :
    Schema PokemonSchema
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


trainerSchema :
    Schema x
        { id : Attribute Int
        , name : Attribute String
        , image : Attribute String
        , pokemon : Relationship HasMany PokemonSchema
        }
trainerSchema =
    Rest.schema "trainers"
        { id = Rest.int "id"
        , name = Rest.string "name"
        , image = Rest.string "image"
        , pokemon = Rest.hasMany "captures"
        }


type alias Model =
    List Trainer


type Msg
    = Fetch (Result Http.Error (List Trainer))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch (Ok trainer) ->
            ( trainer, Cmd.none )

        _ ->
            ( model, Cmd.none )


viewTrainer : Trainer -> Html Msg
viewTrainer trainer =
    div []
        [ div [ style "display" "inline-block", style "vertical-align" "middle" ]
            [ div [ style "text-align" "center" ] [ text trainer.name ]
            , img [ src trainer.image ] []
            ]
        , div [ style "display" "inline-block", style "vertical-align" "middle" ]
            (List.map viewPokemon trainer.pokemon)
        ]


viewPokemon : Pokemon -> Html Msg
viewPokemon pokemon =
    div [ style "display" "inline-block" ]
        [ img [ src pokemon.image ] []
        , div [ style "text-align" "center" ] [ text ("#" ++ String.fromInt pokemon.id ++ ": " ++ pokemon.name) ]
        ]


view : Model -> Html Msg
view model =
    div [] (List.map viewTrainer model)


page : Model -> { title : String, body : List (Html Msg) }
page model =
    { title = "Relationship Example"
    , body = [ view model ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( [], getTrainers )
        , view = page
        , update = update
        , subscriptions = \_ -> Sub.none
        }
