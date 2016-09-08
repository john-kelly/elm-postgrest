module PostgRest.Infix exposing ((.))

{-| PostgREST Query Builder
@docs (.)
-}

import PostgRest exposing (Query, Field, subQuery)


{-| -}
(.) : Query s -> a -> Field
(.) =
    subQuery
