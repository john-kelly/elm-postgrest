module RelationshipExample exposing (..)

import PostgRest as PG
    exposing
        ( Attribute
        , Request
        , Schema
        , Selection
        , Relationship
        , HasMany
        )
import Html exposing (Html, img, div, text)
import Html.Attributes exposing (src, style)
import Http


getTrainers : Cmd Msg
getTrainers =
    PG.readAll trainerSchema trainerSelection
        |> PG.toHttpRequest
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
    PG.map3 Pokemon
        (PG.field .image)
        (PG.field .name)
        (PG.field .id)


trainerSelection :
    Selection
        { attributes
            | image : Attribute String
            , name : Attribute String
            , id : Attribute Int
            , pokemon : Relationship HasMany PokemonSchema
        }
        Trainer
trainerSelection =
    let
        pokemonEmbed =
            -- NOTE: could really use an embedAll here...
            (PG.embedMany .pokemon pokemonSchema)
                { select = pokemonSelection
                , where_ = PG.true
                , order = []
                , limit = Nothing
                , offset = Nothing
                }
    in
        PG.map3 Trainer
            (PG.field .name)
            (PG.field .image)
            pokemonEmbed


type PokemonSchema
    = PokemonSchema PokemonSchema


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


trainerSchema :
    Schema x
        { id : Attribute Int
        , name : Attribute String
        , image : Attribute String
        , pokemon : Relationship HasMany PokemonSchema
        }
trainerSchema =
    PG.schema "trainers"
        { id = PG.int "id"
        , name = PG.string "name"
        , image = PG.string "image"
        , pokemon = PG.hasMany "captures"
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


view : Model -> Html Msg
view model =
    div [] (List.map viewTrainer model)


viewTrainer : Trainer -> Html Msg
viewTrainer trainer =
    div []
        [ div [ style [ ( "display", "inline-block" ), ( "vertical-align", "middle" ) ] ]
            [ div [ style [ ( "text-align", "center" ) ] ] [ text trainer.name ]
            , img [ src trainer.image ] []
            ]
        , div [ style [ ( "display", "inline-block" ), ( "vertical-align", "middle" ) ] ]
            (List.map viewPokemon trainer.pokemon)
        ]


viewPokemon : Pokemon -> Html Msg
viewPokemon pokemon =
    div [ style [ ( "display", "inline-block" ) ] ]
        [ img [ src pokemon.image ] []
        , div [ style [ ( "text-align", "center" ) ] ] [ text ("#" ++ toString pokemon.id ++ ": " ++ pokemon.name) ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( [], getTrainers )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
