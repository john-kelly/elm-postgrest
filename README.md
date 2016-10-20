# elm-postgrest

A query builder library for PostgREST.

## Example

```elm
import Task
import PostgRest exposing (..)
import Json.Decode as Decode

pokemonResource =
    resource "pokemon"
        { id = int "id"
        , name = string "name"
        , base_experience = int "base_experience"
        , weight = int "weight"
        , height = int "height"
        }


type alias Pokemon =
    { id : Int
    , name : String
    }


type Msg
    = FetchSucceed (List Pokemon)
    | FetchFail Http.Error


pokemonCmd =
    query pokemonResource Pokemon
        |> select .id
        |> select .name
        |> filter [ .id |> lte 151 ]
        |> order [ asc .id ]
        |> list Nothing "http://localhost:8000/"
        |> Task.perform FetchFail FetchSucceed
```
