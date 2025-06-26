module Decode exposing (Exposition, expositionsDecoder)

import Json.Decode exposing (..)


type alias Exposition =
    { abstract : String

    --, defaultPage : String
    , id : Int

    --, keywords : List String
    --, publication : List Publication
    , author : Author
    , thumb : Maybe String
    , title : String
    , url : String
    , issue : Maybe Issue

    --, type_ : String
    }


type alias Author =
    { id : Int
    , name : String
    }


type alias Issue =
    { id : Int
    , number : String
    , title : String
    }


type alias Publication =
    { id : Int
    , name : String
    }


publicationDecoder : Json.Decode.Decoder Publication
publicationDecoder =
    Json.Decode.map2 Publication
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)


authorDecoder : Json.Decode.Decoder Author
authorDecoder =
    Json.Decode.map2 Author
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)


issueDecoder : Json.Decode.Decoder Issue
issueDecoder =
    Json.Decode.map3 Issue
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "number" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)


expositionDecoder : Json.Decode.Decoder Exposition
expositionDecoder =
    Json.Decode.map7 Exposition
        (Json.Decode.field "abstract" Json.Decode.string)
        --(Json.Decode.field "default-page" Json.Decode.string)
        (Json.Decode.field "id" Json.Decode.string
            |> Json.Decode.andThen
                (\str ->
                    case String.toInt str of
                        Just int ->
                            Json.Decode.succeed int

                        -- Successfully convert the string to an Int
                        Nothing ->
                            Json.Decode.fail "Expected a string that can be converted to an Int"
                )
        )
        --(Json.Decode.field "keywords" <| Json.Decode.list Json.Decode.string)
        --(Json.Decode.field "published_in" <| Json.Decode.list publicationDecoder)
        (Json.Decode.field "author" authorDecoder)
        (Json.Decode.maybe (Json.Decode.field "thumb" Json.Decode.string))
        --(Json.Decode.field "meta-data-page" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "default-page" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "issue" issueDecoder))


expositionsDecoder : Json.Decode.Decoder (List Exposition)
expositionsDecoder =
    Json.Decode.list expositionDecoder
