module Schema exposing (School, school)

import PostgRest exposing (Attribute, Schema, schema, string)


type School
    = School School


school :
    Schema School
        { id : Attribute String
        , name : Attribute String
        , address : Attribute String
        , city : Attribute String
        , state : Attribute String
        , zip : Attribute String
        , web : Attribute String
        , longitude : Attribute String
        , latitude : Attribute String
        }
school =
    schema "schools"
        { id = string "unitid"
        , name = string "instnm"
        , address = string "addr"
        , city = string "city"
        , state = string "stabbr"
        , zip = string "zip"
        , web = string "webaddr"
        , longitude = string "longitud"
        , latitude = string "latitude"
        }
