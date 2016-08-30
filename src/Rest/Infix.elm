module Rest.Infix
    exposing
        ( (.~~)
        , (!~~)
        , (.==)
        , (!==)
        , (.>=)
        , (!>=)
        , (.>)
        , (!>)
        , (.<=)
        , (!<=)
        , (.<)
        , (!<)
        , (.~~*)
        , (!~~*)
        , (.@@)
        , (!@@)
        )

import Rest
    exposing
        ( Property
        , Filter
        , like
        , eq
        , gte
        , gt
        , lte
        , lt
        , neq
        , ilike
        , contains
        , not'
        )


(.~~) : (a -> Property) -> String -> (a -> Filter)
(.~~) =
    -- use flip to support: .title .~~ "P%"
    flip like


(!~~) : (a -> Property) -> String -> (a -> Filter)
(!~~) =
    flip <| not' like


(.~~*) : (a -> Property) -> String -> (a -> Filter)
(.~~*) =
    flip ilike


(!~~*) : (a -> Property) -> String -> (a -> Filter)
(!~~*) =
    flip <| not' ilike


(.==) : (a -> Property) -> String -> (a -> Filter)
(.==) =
    flip eq


(!==) : (a -> Property) -> String -> (a -> Filter)
(!==) =
    flip neq


(.>=) : (a -> Property) -> String -> (a -> Filter)
(.>=) =
    flip gte


(!>=) : (a -> Property) -> String -> (a -> Filter)
(!>=) =
    flip <| not' gte


(.>) : (a -> Property) -> String -> (a -> Filter)
(.>) =
    flip gt


(!>) : (a -> Property) -> String -> (a -> Filter)
(!>) =
    flip <| not' gt


(.<=) : (a -> Property) -> String -> (a -> Filter)
(.<=) =
    flip lte


(!<=) : (a -> Property) -> String -> (a -> Filter)
(!<=) =
    flip <| not' lte


(.<) : (a -> Property) -> String -> (a -> Filter)
(.<) =
    flip lt


(!<) : (a -> Property) -> String -> (a -> Filter)
(!<) =
    flip <| not' lt


(.@@) : (a -> Property) -> String -> (a -> Filter)
(.@@) =
    flip contains


(!@@) : (a -> Property) -> String -> (a -> Filter)
(!@@) =
    flip <| not' contains
