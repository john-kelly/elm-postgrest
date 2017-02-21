# elm-postgrest

[![Build Status](https://travis-ci.org/john-kelly/elm-postgrest.svg?branch=master)](https://travis-ci.org/john-kelly/elm-postgrest)

A query builder library for PostgREST.

## Example

```elm
import PostgRest as PG

type PokemonResource
    = PokemonResource

pokemonResource =
    PG.resource PokemonResource
        "pokemon"
        { id = PG.int "id"
        , name = PG.string "name"
        , base_experience = PG.int "base_experience"
        , weight = PG.int "weight"
        , height = PG.int "height"
        }

type alias Pokemon =
    { id : Int
    , name : String
    }

pokemonRequest =
    PG.query pokemonResource Pokemon
        |> PG.select .id
        |> PG.select .name
        |> PG.filter [ .id |> PG.lte 151 ]
        |> PG.order [ PG.asc .id ]
        |> PG.list PG.noLimit "http://localhost:8000/"
```
