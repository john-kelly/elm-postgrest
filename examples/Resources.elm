module Resources exposing (Session, session, Speaker, speaker)

import PostgRest as PG


type Session
    = Session


session =
    PG.resource Session
        "sessions"
        { id = PG.int "id"
        , speaker_id = PG.int "speaker_id"
        , start_time = PG.string "start_time"
        , end_time = PG.string "end_time"
        , location = PG.string "location"
        , session_type = PG.int "session_type"
        , speaker = PG.hasOne Speaker
        }


type Speaker
    = Speaker


speaker =
    PG.resource Speaker
        "speakers"
        { id = PG.int "id"
        , name = PG.string "name"
        , twitter = PG.nullable (PG.string "twitter")
        , avatar_url = PG.string "avatar_url"
        , bio = PG.nullable (PG.string "bio")
        , featured = PG.bool "featured"
        , lineup_order = PG.nullable (PG.int "lineup_order")
        , session = PG.hasMany Session
        }
