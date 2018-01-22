import PostgRest as PG
    exposing
        ( Attribute
        , Request
        , Schema
        , Selection
        )


getArticles : Request (List Article)
getArticles =
    PG.readAll articleSchema articleSelection


type alias Article =
    { title : String
    , body : String
    , favoritesCount : Int
    }


articleSelection :
    Selection
        { attributes
            | title : Attribute String
            , body : Attribute String
            , favoritesCount : Attribute String
        }
        Article
articleSelection =
    PG.map3 Article
        (PG.field .title)
        (PG.field .body)
        (PG.field .favoritesCount)


articleSchema :
    Schema x
        { title : Attribute String
        , body : Attribute String
        , favoritesCount : Attribute Int
        }
articleSchema =
    PG.schema "articles"
        { title = PG.string "title"
        , body = PG.string "body"
        , favoritesCount = PG.int "favorites_count"
        }
