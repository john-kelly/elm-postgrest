# elm-postgrest

A query builder library for PostgREST.

## Example

```elm
import Task
import PostgRest exposing (..)
import Json.Decode as Decode

pokemonResource =
    resource "pokemon"
        { id = field "id" Decode.int
        , name = field "name" Decode.string
        , base_experience = field "base_experience" Decode.int
        , weight = field "weight" Decode.int
        , height = field "height" Decode.int
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
