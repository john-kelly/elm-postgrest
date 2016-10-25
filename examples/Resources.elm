module Resources exposing (session, speaker)

import PostgRest as PG


session =
    PG.resource "sessions"
        { id = PG.int "id"
        , speaker_id = PG.int "speaker_id"
        , start_time = PG.string "start_time"
        , end_time = PG.string "end_time"
        , location = PG.string "location"
        , session_type = PG.int "session_type"
        }


speaker =
    PG.resource "speakers"
        { id = PG.int "id"
        , name = PG.string "name"
        , twitter = PG.string "twitter"
        , avatar_url = PG.string "avatar_url"
        , bio = PG.string "bio"
        , featured = PG.bool "featured"
        , lineup_order = PG.int "lineup_order"
        }
