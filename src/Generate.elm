module Generate exposing (main)

import Browser exposing (..)
import Dict exposing (Dict)
import Element exposing (Column)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, decodeString, dict, int)
import Portals exposing (portalsList)
import Task exposing (Task)


type alias PortalDict =
    Dict String Int


portalDecoder : Decoder PortalDict
portalDecoder =
    dict int


type alias Model =
    { portal : Maybe Int
    , issue : Maybe Int
    , keyword : String
    , elements : Int
    , order : String
    , portals : PortalDict
    , width : String
    , error : Maybe String
    , expositionID : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        setPortalCmd =
            Task.perform (always (SetPortal "All Portals")) (Task.succeed ())
    in
    ( { portal = Just -999
      , issue = Nothing
      , keyword = ""
      , elements = 2
      , order = "recent"
      , portals = Dict.empty
      , width = "wide"
      , error = Nothing
      , expositionID = Nothing
      }
    , Cmd.batch [ fetchPortals, setPortalCmd ]
    )



-- case decodedPortals of
--     Ok portals ->
--         ( { portal = Just -999, issue = Nothing, width = "wide", keyword = "", elements = 4, order = "recent", portals = portals, error = Nothing }, setPortalCmd )
--     Err error ->
--         ( { portal = Nothing, issue = Nothing, width = "wide", keyword = "", elements = 2, order = "recent", portals = Dict.empty, error = Just (errorToString error) }, Cmd.none )


type Msg
    = Increment
    | Decrement
    | UpdateKeyword String
    | SetOrder String
    | SetIframeWidth String
    | SetPortal String
    | SetIssueID String
    | SetExpositionIDs String
    | FetchData
    | GotPortals (Result Http.Error (Dict String Int))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | elements = model.elements + 1 }, Cmd.none )

        Decrement ->
            ( { model | elements = model.elements - 1 }, Cmd.none )

        UpdateKeyword newKeyword ->
            ( { model | keyword = newKeyword }, Cmd.none )

        SetOrder newOrder ->
            ( { model | order = newOrder }, Cmd.none )

        SetIframeWidth newWidth ->
            ( { model | width = newWidth }, Cmd.none )

        SetPortal portalName ->
            ( let
                newPortal =
                    getPortalId model.portals portalName
              in
              { model
                | portal = newPortal
              }
            , Cmd.none
            )

        SetIssueID issueID ->
            ( { model
                | issue = String.toInt issueID
              }
            , Cmd.none
            )

        SetExpositionIDs expositionIDs ->
            ( { model
                | expositionID = Just expositionIDs -- Store the raw string in the model
              }
            , Cmd.none
            )

        FetchData ->
            ( model, Cmd.none )

        GotPortals result ->
            case result of
                Ok portals ->
                    ( { model | portals = portals, error = Nothing }, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "Error fetching portals" (httpErrorToString error)
                    in
                    ( { model | error = Just (httpErrorToString error) }, Cmd.none )


errorToString : Json.Decode.Error -> String
errorToString error =
    Json.Decode.errorToString error


withSpacing : List (Html msg) -> List (Html msg)
withSpacing =
    List.intersperse (text " ")


view : Model -> Html Msg
view model =
    let
        portalId =
            model.portal

        portalAsString =
            portalIdToString portalId

        issueID =
            case model.issue of
                Just issue ->
                    String.fromInt issue

                Nothing ->
                    ""

        expositionIDsAsList =
            case model.expositionID of
                Just idsString ->
                    String.split "," idsString
                        |> List.filterMap String.toInt

                Nothing ->
                    []

        expositionID =
            case model.expositionID of
                Just ids ->
                    String.join "," (List.map String.fromInt expositionIDsAsList)

                Nothing ->
                    ""

        url =
            "https://rcfeed.rcdata.org/?keyword=" ++ model.keyword ++ "&elements=" ++ String.fromInt model.elements ++ "&order=" ++ model.order ++ "&portal=" ++ portalAsString ++ "&issue=" ++ issueID ++ "&expositionID=" ++ expositionID ++ "&feed=" ++ model.width

        --"http://localhost:8080/?keyword=rc" ++ model.keyword ++ "&elements=" ++ String.fromInt model.elements ++ "&order=" ++ model.order ++ "&portal=" ++ portalAsString ++ "&issue=" ++ issueID ++ "&expositionID=" ++ expositionID ++ "&feed=" ++ model.width
        maxElementsWithTitle =
            if model.width == "column" then
                7

            else
                11

        heightMultiplier =
            if model.width == "column" then
                1100

            else
                2000

        iFrameDiv =
            if model.width == "column" then
                div []
                    [ iframe
                        [ src (url ++ "&mode=generate"), style "width" "100%", style "max-width" "1024px", height iframeHeight ]
                        []
                    ]

            else
                div []
                    [ iframe
                        [ src (url ++ "&mode=generate"), style "width" "100%", height iframeHeight ]
                        []
                    ]

        iframeHeight =
            if model.elements < 3 then
                round (7 / (4 * 3) * heightMultiplier)

            else if model.elements < maxElementsWithTitle then
                round (7 / (4 * toFloat model.elements) * heightMultiplier)

            else
                round (1 / toFloat model.elements * heightMultiplier)

        portalOptions =
            List.map (portalOption model.portals model.portal) (Dict.keys model.portals)
    in
    div [ align "center" ]
        [ div []
            [ h1 []
                [ text "Generate Feed" ]
            ]
        , div []
            (withSpacing
                [ text "Portal: "
                , select [ onInput SetPortal ]
                    portalOptions
                , text "Issue: "
                , input [ placeholder "ID", value issueID, onInput SetIssueID ] []
                , text "Feed: "
                , select [ onInput SetIframeWidth ]
                    (List.map orderOption [ "wide", "column" ])
                ]
            )
        , div []
            (withSpacing
                [ text "Keyword: "
                , input [ placeholder "Type your keyword here", value model.keyword, onInput UpdateKeyword ] []
                , text "Exposition ID: "
                , input
                    [ placeholder "ID"
                    , value (Maybe.withDefault "" model.expositionID) -- Display the raw string in the input field
                    , onInput SetExpositionIDs -- Pass the raw string to the handler
                    ]
                    []
                , text "Number of Elements to Display: "
                , button [ onClick Decrement ] [ text "-" ]
                , text (String.fromInt model.elements)
                , button [ onClick Increment ] [ text "+" ]
                , text "Order of Elements: "
                , select [ onInput SetOrder ]
                    (List.map orderOption [ "recent", "random" ])
                ]
            )
        , br [] []
        , div []
            [ text "Please note that page level CSS is required for the iframe to be responsive. This is available at "
            , a [ attribute "href" "https://github.com/SocietyForArtisticResearch/rc-custom-feed/blob/master/README.md#page-level-css" ]
                [ text "here"
                , text "."
                ]
            ]

        --, div [ style "width" "100%" ] [ citableIframe ("<div class=\"contdiv" ++ String.fromInt model.elements ++ "\"><iframe src=" ++ q url ++ " style=\"border: none;\"></iframe></div>") ]
        , br [] []
        , iFrameDiv
        ]


q : String -> String
q str =
    "\"" ++ str ++ "\""


portalOption : PortalDict -> Maybe Int -> String -> Html msg
portalOption portals selectedPortal portalName =
    let
        -- Check if the current portal is selected
        isSelected =
            case selectedPortal of
                Just selectedId ->
                    -- Retrieve the portal ID from the dictionary and check if it matches
                    case getPortalId portals portalName of
                        Just portalId ->
                            portalId == selectedId

                        Nothing ->
                            False

                Nothing ->
                    False
    in
    option [ value portalName, selected isSelected ] [ text portalName ]


getPortalId : PortalDict -> String -> Maybe Int
getPortalId portals portalName =
    Dict.get portalName portals


orderOption : String -> Html msg
orderOption order =
    option [ value order ] [ text order ]


portalIdToString : Maybe Int -> String
portalIdToString id =
    case id of
        Just int ->
            if int == -999 then
                ""

            else
                String.fromInt int

        Nothing ->
            ""


type alias Portals =
    Dict.Dict String Int


portalsDecoder : Decoder Portals
portalsDecoder =
    Json.Decode.dict Json.Decode.int


fetchPortals : Cmd Msg
fetchPortals =
    Http.get
        { url = "all_portals.json"
        , expect = Http.expectJson GotPortals portalsDecoder
        }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error occurred"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
