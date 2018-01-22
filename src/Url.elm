module Url
    exposing
        ( absolute
        , relative
        , crossOrigin
        , custom
        , Root(..)
        , QueryParameter
        , string
        , int
        , toQuery
        , percentEncode
        , percentDecode
        )

{-| In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:

      https://example.com:8042/over/there?name=ferret#nose
      \___/   \______________/\_________/ \_________/ \__/
        |            |            |            |        |
      scheme     authority       path        query   fragment

This module helps you create these!


# Builders

@docs absolute, relative, crossOrigin, custom, Root


# Queries

@docs QueryParameter, string, int, toQuery


# Percent-Encoding

@docs percentEncode, percentDecode

-}

import Http


{-| Create an absolute URL:

    absolute [] []
    -- "/"

    absolute [ "packages", "elm-lang", "core" ] []
    -- "/packages/elm-lang/core"

    absolute [ "blog", String.fromInt 42 ] []
    -- "/blog/42"

    absolute [ "products" ] [ string "search" "hat", int "page" 2 ]
    -- "/products?search=hat&page=2"

Notice that the URLs start with a slash!

-}
absolute : List String -> List QueryParameter -> String
absolute pathSegments parameters =
    "/" ++ String.join "/" pathSegments ++ toQuery parameters


{-| Create a relative URL:

    relative [] []
    -- ""

    relative [ "elm-lang", "core" ] []
    -- "elm-lang/core"

    relative [ "blog", String.fromInt 42 ] []
    -- "blog/42"

    relative [ "products" ] [ string "search" "hat", int "page" 2 ]
    -- "products?search=hat&page=2"

Notice that the URLs **do not** start with a slash!

-}
relative : List String -> List QueryParameter -> String
relative pathSegments parameters =
    String.join "/" pathSegments ++ toQuery parameters


{-| Create a cross-origin URL.

    crossOrigin "https://example.com" [ "products" ] [] Nothing
    -- "https://example.com/products"

    crossOrigin "https://example.com" [] [] (Just "help")
    -- "https://example.com/#help"

    crossOrigin
      "https://example.com:8042"
      [ "over", "there" ]
      [ string "name" "ferret" ]
      (Just "nose")
    -- "https://example.com:8042/over/there?name=ferret#nose"

**Note:** Cross-origin requests are slightly restricted for security.
For example, the [same-origin policy][sop] applies when sending HTTP requests,
so the appropriate `Access-Control-Allow-Origin` header must be enabled on the
*server* to get things working. Read more about the security rules [here][cors].

[sop]: https://developer.mozilla.org/en-US/docs/Web/Security/Same-origin_policy
[cors]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS

-}
crossOrigin : String -> List String -> List QueryParameter -> String
crossOrigin prePath pathSegments parameters =
    prePath ++ "/" ++ String.join "/" pathSegments ++ toQuery parameters


{-| Specify whether a [`custom`](#custom) URL is absolute, relative, or
cross-origin.
-}
type Root
    = Absolute
    | Relative
    | CrossOrigin String


{-| Create custom URLs that may have a hash on the end:

    custom Absolute
      [ "packages", "elm-lang", "core", "latest", "String" ]
      []
      (Just "length")
    -- "/packages/elm-lang/core/latest/String#length"

    custom Relative [ "there" ] [ string "name" "ferret" ] Nothing
    -- "there?name=ferret"

    custom
      (CrossOrigin "https://example.com:8042")
      [ "over", "there" ]
      [ string "name" "ferret" ]
      (Just "nose")
    -- "https://example.com:8042/over/there?name=ferret#nose"

-}
custom : Root -> List String -> List QueryParameter -> Maybe String -> String
custom root pathSegments parameters maybeFragment =
    let
        fragmentless =
            rootToPrePath root ++ String.join "/" pathSegments ++ toQuery parameters
    in
        case maybeFragment of
            Nothing ->
                fragmentless

            Just fragment ->
                fragmentless ++ "#" ++ fragment


rootToPrePath : Root -> String
rootToPrePath root =
    case root of
        Absolute ->
            "/"

        Relative ->
            ""

        CrossOrigin prePath ->
            prePath



-- QUERY PARAMETERS


{-| Represents query parameter. Builder functions like `absolute` percent-encode
all the query parameters they get, so you do not need to worry about it!
-}
type QueryParameter
    = QueryParameter String String


{-| Create a percent-encoded query parameter.

    absolute ["products"] [ string "search" "hat" ]
    -- "/products?search=hat"

    absolute ["products"] [ string "search" "coffee table" ]
    -- "/products?search=coffee%20table"

-}
string : String -> String -> QueryParameter
string key value =
    QueryParameter (percentEncode key) (percentEncode value)


{-| Create a percent-encoded query parameter.

    absolute ["products"] [ string "search" "hat", int "page" 2 ]
    -- "/products?search=hat&page=2"

Writing `int key n` is the same as writing `string key (String.fromInt n)`.
So this is just a convenience function, making your code a bit shorter!

-}
int : String -> Int -> QueryParameter
int key int =
    QueryParameter (percentEncode key) (toString int)


{-| Convert a list of query parameters to a percent-encoded query. This
function is used by `absolute`, `relative`, etc.

    toQuery [ string "search" "hat" ]
    -- "?search=hat"

    toQuery [ string "search" "coffee table" ]
    -- "?search=coffee%20table"

    toQuery [ string "search" "hat", int "page" 2 ]
    -- "?search=hat&page=2"

    toQuery []
    -- ""

-}
toQuery : List QueryParameter -> String
toQuery parameters =
    case parameters of
        [] ->
            ""

        _ ->
            "?" ++ String.join "&" (List.map toQueryPair parameters)


toQueryPair : QueryParameter -> String
toQueryPair (QueryParameter key value) =
    key ++ "=" ++ value



-- PERCENT ENCODING


{-| Percent-encoding is how [the official URI spec][uri] “escapes” special
characters. You can still represent a `?` even though it is reserved for
queries. **All of the `absolute`, `relative`, `crossOrigin`, and `custom`
functions already do this automatically!** Do not do it twice.

This function exists in case you want to do something extra custom. Here are
some examples:

    -- standard ASCII encoding
    percentEncode "hat"   == "hat"
    percentEncode "to be" == "to%20be"
    percentEncode "99%"   == "99%25"

    -- non-standard, but widely accepted, UTF-8 encoding
    percentEncode "$" == "%24"
    percentEncode "¢" == "%C2%A2"
    percentEncode "€" == "%E2%82%AC"

This is the same behavior as JavaScript's [`encodeURIComponent`][js] function,
and the rules are described in more detail officially [here][s2] and with some
notes about Unicode [here][wiki].

[js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent
[uri]: https://tools.ietf.org/html/rfc3986
[s2]: https://tools.ietf.org/html/rfc3986#section-2.1
[wiki]: https://en.wikipedia.org/wiki/Percent-encoding

-}
percentEncode : String -> String
percentEncode =
    Http.encodeUri


{-| Check out the `percentEncode` function to learn about percent-encoding.
This function does the opposite! Here are the reverse examples:

    -- ASCII
    percentDecode "99%25"     == Just "hat"
    percentDecode "to%20be"   == Just "to be"
    percentDecode "hat"       == Just "99%"

    -- UTF-8
    percentDecode "%24"       == Just "$"
    percentDecode "%C2%A2"    == Just "¢"
    percentDecode "%E2%82%AC" == Just "€"

Why is it a `Maybe` though? Well, these strings come from strangers on the
internet as a bunch of bits and may have encoding problems. For example:

    percentDecode "%"   == Nothing  -- not followed by two hex digits
    percentDecode "%XY" == Nothing  -- not followed by two HEX digits
    percentDecode "%C2" == Nothing  -- half of the "¢" encoding "%C2%A2"

This is the same behavior as JavaScript's [`decodeURIComponent`][js] function.

[js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent

-}
percentDecode : String -> Maybe String
percentDecode =
    Http.decodeUri
