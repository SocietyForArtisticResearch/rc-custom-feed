module Decode exposing (Author, Exposition, expositionsDecoder)

import Json.Decode exposing (..)


type alias Exposition =
    { abstract : String
    , id : Int
    , author : Author
    , coauthors : List Author
    , thumb : Maybe String
    , title : String
    , url : String
    , issue : Maybe Issue
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
    Json.Decode.map8 Exposition
        (Json.Decode.field "abstract" Json.Decode.string)
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "author" authorDecoder)
        (Json.Decode.field "coauthors" (Json.Decode.list authorDecoder))
        (Json.Decode.maybe (Json.Decode.field "thumb" Json.Decode.string))
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "default-page" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "issue" issueDecoder))


expositionsDecoder : Json.Decode.Decoder (List Exposition)
expositionsDecoder =
    Json.Decode.list expositionDecoder
