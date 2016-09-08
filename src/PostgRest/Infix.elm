module PostgRest.Infix exposing ((.))

{-| PostgREST Infix Operators
@docs (.)
-}

import PostgRest exposing (Query, Field, subQuery)


{-| -}
(.) : Query s -> a -> Field
(.) =
    subQuery
