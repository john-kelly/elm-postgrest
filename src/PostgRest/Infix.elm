module PostgRest.Infix exposing ((.))

{-| -}

import PostgRest exposing (Query, Field, subQuery)


{-| -}
(.) : Query s -> a -> Field
(.) =
    subQuery
