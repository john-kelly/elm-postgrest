module Query.Types exposing (..)

{-| -}


{-| -}
type Schema s
    = Schema String s


{-| -}
unwrapSchema : Schema s -> ( String, s )
unwrapSchema schema =
    case schema of
        Schema name shape ->
            ( name, shape )


{-| -}
type Query s
    = Query String s QueryParams


unwrapQuery : Query s -> ( String, s, QueryParams )
unwrapQuery query =
    case query of
        Query name shape params ->
            ( name, shape, params )


type alias QueryParams =
    { select : List Field
    , order : List OrderBy
    , filter : List Filter
    }


{-| -}
type Field
    = Simple String
    | Nested String QueryParams


{-| -}
type OrderBy
    = Asc Field
    | Desc Field


{-| -}
type Condition
    = Like String
    | ILike String
    | Eq String
    | Gte String
    | Gt String
    | Lte String
    | Lt String
    | In (List String)
    | Is String


{-| Is it possible to make the illegal state of a Filter on a Nested Field Unrepresentable?
https://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable/
https://blogs.janestreet.com/effective-ml-revisited/
-}
type Filter
    = Filter Bool Condition Field
