module Query.Infix exposing ((.))

{-| -}

import Query exposing (Query, Field, subQuery)


{-| -}
(.) : Query s -> a -> Field
(.) =
    subQuery
