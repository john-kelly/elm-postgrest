import PostgRest as PG
    exposing
        ( Attribute
        , HasOne
        , Relationship
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
    , author : Author
    }


articleSelection :
    Selection
        { attributes
            | title : Attribute String
            , body : Attribute String
            , favoritesCount : Attribute String
            , author : Relationship HasOne UserSchema
        }
        Article
articleSelection =
    PG.map4 Article
        (PG.field .title)
        (PG.field .body)
        (PG.field .favoritesCount)
        (PG.embedOne .author userSchema authorSelection)


type alias Author =
    { name : String
    , image : String
    }


authorSelection :
    Selection
        { attributes
            | name : Attribute String
            , image : Attribute String
        }
        Author
authorSelection =
    PG.map2 Author
        (PG.field .name)
        (PG.field .image)


articleSchema :
    Schema x
        { title : Attribute String
        , body : Attribute String
        , favoritesCount : Attribute Int
        , author : Relationship HasOne UserSchema
        }
articleSchema =
    PG.schema "articles"
        { title = PG.string "title"
        , body = PG.string "body"
        , favoritesCount = PG.int "favorites_count"
        , author = PG.hasOne "author_id"
        }


type UserSchema
    = UserSchema UserSchema


userSchema :
    Schema UserSchema
        { name : Attribute String
        , bio : Attribute String
        , image : Attribute String
        }
userSchema =
    PG.schema "users"
        { name = PG.string "name"
        , bio = PG.string "bio"
        , image = PG.string "image"
        }
