module Main exposing (main, release, shuffleWithSeed)

import AppUrl exposing (AppUrl)
import Array
import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Carousel exposing (Carousel)
import Decode exposing (..)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (br)
import Html.Attributes
import Http
import Json.Decode exposing (Error(..))
import Random
import Random.List exposing (shuffle)
import Simple.Transition as Transition
import String.Extra
import Url exposing (Url)


columnRatios =
    Array.fromList [ 7 / 12, 7 / 8, 7 / 12, 7 / 16, 7 / 20, 7 / 24 ]


wideRatios =
    Array.fromList [ 7 / 12, 7 / 12, 7 / 13, 7 / 17, 7 / 21, 7 / 25 ]


type Release
    = Live
    | Development


type Feed
    = Wide
    | Column


type Mode
    = Generate
    | Display


stringToFeed : Maybe String -> Feed
stringToFeed string =
    case string of
        Just s ->
            case s of
                "wide" ->
                    Wide

                "column" ->
                    Column

                _ ->
                    Column

        _ ->
            Column


stringToMode : Maybe String -> Mode
stringToMode string =
    case string of
        Just s ->
            case s of
                "generate" ->
                    Generate

                "display" ->
                    Display

                _ ->
                    Display

        _ ->
            Display


baseUrl r =
    case r of
        Live ->
            "https://rcfeed.rcdata.org/"

        Development ->
            "http://localhost:8080/"


parametersFromAppUrl : AppUrl -> Parameters
parametersFromAppUrl url =
    Parameters
        (Dict.get
            "keyword"
            url.queryParameters
            |> Maybe.andThen List.head
        )
        (Dict.get "elements" url.queryParameters
            |> Maybe.andThen List.head
            |> Maybe.andThen String.toInt
        )
        (Dict.get
            "order"
            url.queryParameters
            |> Maybe.andThen List.head
        )
        (Dict.get
            "portal"
            url.queryParameters
            |> Maybe.andThen List.head
        )
        (Dict.get
            "issue"
            url.queryParameters
            |> Maybe.andThen List.head
            |> Maybe.andThen String.toInt
        )
        (Dict.get
            "expositionID"
            url.queryParameters
            |> Maybe.andThen List.head
            |> Maybe.andThen
                (\str ->
                    if String.isEmpty str then
                        Nothing
                        -- Return `Nothing` if the string is empty

                    else
                        Just
                            (String.split "," str
                                |> List.filterMap String.toInt
                             -- Convert each value to an Int, ignoring invalid entries
                            )
                )
        )
        (Dict.get
            "feed"
            url.queryParameters
            |> Maybe.andThen List.head
            |> stringToFeed
        )
        (Dict.get
            "mode"
            url.queryParameters
            |> Maybe.andThen List.head
            |> stringToMode
        )


shuffleWithSeed : Int -> List a -> List a
shuffleWithSeed seed lst =
    Random.initialSeed seed
        |> Random.step (shuffle lst)
        |> Tuple.first


type Msg
    = NoOp
    | ChangeUrl Url
    | RequestUrl Browser.UrlRequest
    | NextExposition
    | PreviousExposition
    | DataReceived (Result Http.Error (List Exposition))
    | WindowWasResized Int Int


type alias Model =
    { navKey : Nav.Key
    , expositions : Carousel Exposition
    , parameters : Parameters
    , seed : Int
    , view : View
    , windowSize : { w : Int, h : Int }
    , release : Release
    , results : Int
    }


type View
    = Error
    | Carousel


type alias Parameters =
    { keyword : Maybe String
    , elements : Maybe Int
    , order : Maybe String
    , portal : Maybe String
    , issue : Maybe Int
    , expositionID : Maybe (List Int)
    , feed : Feed
    , mode : Mode
    }


type alias Config =
    { seed : Int
    , width : Int
    , height : Int
    , release : Release
    }


defaultConfig =
    { seed = 42
    , width = 1280
    , height = 600
    , release = Live
    }


release : String -> Release
release str =
    case String.toLower str of
        "live" ->
            Live

        "devel" ->
            Development

        "development" ->
            Development

        _ ->
            Development


configDecoder : Json.Decode.Decoder Config
configDecoder =
    Json.Decode.map4
        Config
        (Json.Decode.field "seed" Json.Decode.int)
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)
        (Json.Decode.field "release" Json.Decode.string |> Json.Decode.map release)



-- init ( seed, width, height, release ) url navKey =


init : Json.Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init config url navKey =
    let
        cfg =
            case Json.Decode.decodeValue configDecoder config of
                Result.Ok c ->
                    let
                        _ =
                            Debug.log "release" c.release
                    in
                    c

                Result.Err e ->
                    let
                        _ =
                            Debug.log "invalid config" e
                    in
                    defaultConfig

        model =
            { navKey = navKey
            , expositions = Carousel.create [] 1
            , parameters = parametersFromAppUrl (AppUrl.fromUrl url)
            , view = Carousel
            , seed = cfg.seed
            , windowSize = { w = cfg.width, h = cfg.height }
            , release = cfg.release
            , results = 0
            }

        --_ =
        --    Debug.log "model" model
    in
    ( model, sendQuery cfg.release (Maybe.withDefault "Nothing" model.parameters.keyword) (Maybe.withDefault "Nothing" model.parameters.portal) )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeUrl _ ->
            ( model, Cmd.none )

        RequestUrl _ ->
            ( model, Cmd.none )

        DataReceived (Ok exps) ->
            let
                --_ =
                --    Debug.log "parameters" model.parameters
                filteredByIssue =
                    case model.parameters.issue of
                        Just id ->
                            List.filter (isExpositionInIssue id) exps

                        Nothing ->
                            exps

                expositions =
                    case model.parameters.expositionID of
                        Just expoIds ->
                            List.filter (\exposition -> List.any (\expoId -> isExpositionWithId expoId exposition) expoIds) filteredByIssue

                        Nothing ->
                            filteredByIssue

                --_ =
                --    Debug.log "filtered expositions" expositions
                exp =
                    case model.parameters.order of
                        Just "recent" ->
                            expositions

                        Just "random" ->
                            shuffleWithSeed model.seed expositions

                        Nothing ->
                            expositions

                        Just _ ->
                            expositions

                results =
                    List.length expositions
            in
            ( { model
                | expositions = Carousel.create exp (Maybe.withDefault 1 model.parameters.elements)
                , results = results
              }
            , Cmd.none
            )

        DataReceived (Err error) ->
            let
                _ =
                    Debug.log "error" error
            in
            ( { model | view = Error }, Cmd.none )

        NextExposition ->
            ( { model | expositions = Carousel.next model.expositions }, Cmd.none )

        PreviousExposition ->
            ( { model | expositions = Carousel.previous model.expositions }, Cmd.none )

        WindowWasResized w h ->
            ( { model | windowSize = { w = w, h = h } }, Cmd.none )


sendQuery : Release -> String -> String -> Cmd Msg
sendQuery releaseType keyw portal =
    let
        -- this should later be:
        -- request = "https://www.researchcatalogue.net/portal/search-result?fulltext=&title=&autocomplete=&keyword="
        -- ++ keyw
        -- "&portal=&statusprogress=0&statuspublished=0&includelimited=0&includelimited=1&includeprivate=0&includeprivate=1&type_research=research&resulttype=research&modifiedafter=&modifiedbefore=&format=json&limit=50&page=0"
        url =
            case releaseType of
                Live ->
                    "rcproxy/proxy?keyword=" ++ keyw ++ "&portal=" ++ portal

                Development ->
                    "http://localhost:2019/" ++ keyw

        _ =
            Debug.log "send query :" url
    in
    Http.get
        { url = url
        , expect = Http.expectJson DataReceived Decode.expositionsDecoder
        }


isExpositionInIssue : Int -> Exposition -> Bool
isExpositionInIssue issueID exp =
    case exp.issue of
        Just iss ->
            iss.id == issueID

        Nothing ->
            False


isExpositionWithId : Int -> Exposition -> Bool
isExpositionWithId expoId exposition =
    exposition.id == expoId



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        elements =
            Maybe.withDefault 2 model.parameters.elements

        elem =
            if model.results < elements then
                model.results

            else
                elements

        content =
            case model.view of
                Carousel ->
                    [ layout [ width fill ]
                        -- breakpoint mobile view
                        (if model.windowSize.w > 675 then
                            Carousel.view
                                { carousel = model.expositions
                                , onNext = NextExposition
                                , viewSlide = viewResearch model model.windowSize.w elem model.parameters.feed
                                , num = elem - 1
                                }

                         else
                            Carousel.view
                                { carousel = model.expositions
                                , onNext = NextExposition
                                , viewSlide = viewResearch model model.windowSize.w 1 model.parameters.feed
                                , num = 0
                                }
                        )
                    ]

                Error ->
                    let
                        url =
                            baseUrl model.release
                    in
                    [ layout [ width fill ] (Element.text <| "bad query. you must provide keyword, number of elements and order. example: " ++ url ++ "?keyword=rc&elements=20&order=recent") ]
    in
    { title = "custom-feed"
    , body = content
    }


defaultPadding =
    { top = 0, bottom = 0, left = 0, right = 0 }


maybeStrToStr : Maybe String -> String
maybeStrToStr str =
    case str of
        Just s ->
            s

        Nothing ->
            ""


viewResearch : Model -> Int -> Int -> Feed -> List (Maybe Exposition) -> List (Element Msg)
viewResearch model wi columns feed exp =
    let
        _ =
            Debug.log "feed: " feed

        viewExposition =
            case feed of
                Wide ->
                    viewExpositionWide

                Column ->
                    viewExpositionColumn

        buttonWidth =
            30

        w =
            min wi (columns * 675)

        -- preserve traditional block layout
        columnRatio =
            if columns > 6 then
                toFloat w / toFloat columns

            else if columns < 3 then
                toFloat w * (7 / toFloat (3 * 4))

            else
                toFloat w * (7 / toFloat (columns * 4))

        --Maybe.withDefault 2.0 (Array.get (columns - 1) columnRatios)
        wideRatio =
            if columns > 10 then
                toFloat w / toFloat columns

            else if columns < 3 then
                toFloat w * (7 / toFloat (3 * 4))

            else
                toFloat w * (7 / toFloat (columns * 4))

        --Maybe.withDefault 2.0 (Array.get (columns - 1) wideRatios)
        ratio =
            case feed of
                Wide ->
                    wideRatio

                Column ->
                    columnRatio

        heightt =
            if w > 675 then
                round ratio
                -- feed dimensions

            else
                round (toFloat w * 5 / 4)

        elem =
            Maybe.withDefault 2 model.parameters.elements

        displayedElements =
            min elem model.results

        ratios =
            case model.parameters.feed of
                Column ->
                    columnRatios

                Wide ->
                    wideRatios

        paddingTop =
            if displayedElements < 3 then
                (7 / toFloat (3 * 4) * 100) + 1

            else if displayedElements < 7 then
                --let
                --    val =
                --        Array.get (displayedElements - 1) ratios
                --in
                (7 / toFloat (columns * 4) * 100) + 1

            else
                (1 / toFloat displayedElements * 100) + 1

        style =
            "\"position: relative; overflow: hidden; width: 100%; padding-top: " ++ String.fromFloat paddingTop ++ "%;\""

        div =
            "<div class=\"cont\" style=" ++ style ++ "><iframe src=\""

        endDiv =
            "\" style=\"position: absolute; top: 0; left: 0; bottom: 0; right: 0; width: 100%; height: 100%;\"></iframe></div>"

        issue =
            case model.parameters.issue of
                Just issu ->
                    String.fromInt issu

                Nothing ->
                    ""

        kw =
            maybeStrToStr model.parameters.keyword

        order =
            maybeStrToStr model.parameters.order

        portal =
            maybeStrToStr model.parameters.portal

        expoIDs =
            model.parameters.expositionID
                |> Maybe.map (\ids -> String.join "," (List.map String.fromInt ids))
                |> maybeStrToStr

        url =
            "https://rcfeed.rcdata.org/?keyword=" ++ kw ++ "&elements=" ++ String.fromInt elem ++ "&order=" ++ order ++ "&portal=" ++ portal ++ "&issue=" ++ issue ++ "&expositionID=" ++ expoIDs

        fullUrl =
            div ++ url ++ endDiv
    in
    case model.parameters.mode of
        Generate ->
            [ Element.column [ width fill ]
                [ Element.row [ Element.centerX ] [ paragraph [] [ el [ Font.size 12, Font.bold ] (text ("Found " ++ String.fromInt model.results ++ " expositions matching your search criteria. ")) ] ]
                , Element.row [ Element.centerX, Border.color (rgb255 0 0 0), Border.width 2 ] [ el [ Font.size 12 ] (text fullUrl) ]
                , Element.row []
                    [ paragraph []
                        [ html <| br [] []
                        ]
                    ]
                , Element.row
                    [ width (fill |> maximum w) -- preserve traditional block layout
                    , height (px heightt)
                    , paddingEach { defaultPadding | left = 0 }
                    , spacing 25
                    , Element.clip
                    , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")

                    --, Border.color (rgb255 255 0 0)
                    --, Border.width 2
                    --, Border.rounded 3
                    , Element.alignTop
                    ]
                    (List.concat
                        [ if model.results <= elem then
                            []

                          else
                            [ Input.button
                                [ width <| px buttonWidth
                                , height fill
                                , Element.focused
                                    [ Border.shadow { color = rgb255 1 1 1, offset = ( 0, 0 ), blur = 0, size = 0 } ]
                                , centerX
                                , Element.mouseOver
                                    [ Background.color (Element.rgb 0.97 0.97 0.97)
                                    ]
                                , Transition.properties
                                    [ Transition.backgroundColor 500 []
                                    ]
                                    |> Element.htmlAttribute
                                ]
                                { onPress = Just PreviousExposition
                                , label = Element.image [ width (px 25), height (px 25), rotate 22 ] { src = "assets/shevron.svg", description = "next slide" }
                                }
                            ]
                        , List.map
                            (columns |> (w |> viewExposition))
                            exp
                        , if model.results <= elem then
                            []

                          else
                            [ Input.button
                                [ width <| px buttonWidth
                                , height fill
                                , Element.focused
                                    [ Border.shadow { color = rgb255 1 1 1, offset = ( 0, 0 ), blur = 0, size = 0 } ]
                                , centerX
                                , Element.mouseOver
                                    [ Background.color (Element.rgb 0.97 0.97 0.97)
                                    ]
                                , Transition.properties
                                    [ Transition.backgroundColor 500 []
                                    ]
                                    |> Element.htmlAttribute
                                ]
                                { onPress = Just NextExposition
                                , label = Element.image [ width (px 25), height (px 25) ] { src = "assets/shevron.svg", description = "next slide" }
                                }
                            ]
                        ]
                    )
                ]
            ]

        Display ->
            [ Element.row
                [ width (fill |> maximum w) -- preserve traditional block layout
                , height (px heightt)
                , paddingEach { defaultPadding | left = 0 }
                , spacing 25
                , Element.clip
                , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")

                --, Border.color (rgb255 255 0 0)
                --, Border.width 2
                --, Border.rounded 3
                , Element.alignTop
                ]
                (List.concat
                    [ if model.results <= elem then
                        []

                      else
                        [ Input.button
                            [ width <| px buttonWidth
                            , height fill
                            , Element.focused
                                [ Border.shadow { color = rgb255 1 1 1, offset = ( 0, 0 ), blur = 0, size = 0 } ]
                            , centerX
                            , Element.mouseOver
                                [ Background.color (Element.rgb 0.97 0.97 0.97)
                                ]
                            , Transition.properties
                                [ Transition.backgroundColor 500 []
                                ]
                                |> Element.htmlAttribute
                            ]
                            { onPress = Just PreviousExposition
                            , label = Element.image [ width (px 25), height (px 25), rotate 22 ] { src = "assets/shevron.svg", description = "next slide" }
                            }
                        ]
                    , List.map
                        (columns |> (w |> viewExposition))
                        exp
                    , if model.results <= elem then
                        []

                      else
                        [ Input.button
                            [ width <| px buttonWidth
                            , height fill
                            , Element.focused
                                [ Border.shadow { color = rgb255 1 1 1, offset = ( 0, 0 ), blur = 0, size = 0 } ]
                            , centerX
                            , Element.mouseOver
                                [ Background.color (Element.rgb 0.97 0.97 0.97)
                                ]
                            , Transition.properties
                                [ Transition.backgroundColor 500 []
                                ]
                                |> Element.htmlAttribute
                            ]
                            { onPress = Just NextExposition
                            , label = Element.image [ width (px 25), height (px 25) ] { src = "assets/shevron.svg", description = "next slide" }
                            }
                        ]
                    ]
                )
            ]


defaultPageFromUrl : String -> String
defaultPageFromUrl str =
    let
        expositionUrl =
            Url.fromString str

        expositionPath =
            case expositionUrl of
                Just url ->
                    url.path
                        |> String.dropLeft 5

                _ ->
                    ""
    in
    expositionPath


authorLinks : Author -> List Author -> List (Element Msg)
authorLinks author coauthors =
    let
        mainLink =
            Element.newTabLink []
                { url = authorLink author.id
                , label = Element.text (Debug.log "Main author" author |> .name)
                }

        coauthorLinks =
            coauthors
                |> Debug.log "Coauthors"
                |> List.map
                    (\a ->
                        Element.newTabLink []
                            { url = authorLink a.id
                            , label = Element.text a.name
                            }
                    )
    in
    List.intersperse (Element.text ", ") (mainLink :: coauthorLinks)


viewTitle : Int -> Int -> Maybe Exposition -> Element Msg
viewTitle w columns exp =
    let
        fontSize =
            --max 15 (round (toFloat w / toFloat columns / 20))
            min 20 (max 10 (round (toFloat w / toFloat columns / 20) - columns))

        --22 - columns
        --round (toFloat w / toFloat columns / 20 + 3)
    in
    case exp of
        Just exposition ->
            column
                [ --Border.color (rgb255 0 0 0)
                  --, Border.width 2
                  --, Border.rounded 3
                  Element.centerX
                , spacing 10
                , paddingXY 5 25
                ]
                [ paragraph
                    [ height fill
                    , Font.center
                    , Font.size fontSize
                    , Font.bold
                    ]
                    [ Element.newTabLink
                        []
                        { url = exposition.url
                        , label = Element.text exposition.title
                        }
                    ]
                ]

        Nothing ->
            Element.text "Waiting for exposition..."


viewTitleAuthor : Int -> Int -> Maybe Exposition -> Element Msg
viewTitleAuthor w columns exp =
    let
        fontSize =
            --max 15 (round (toFloat w / toFloat columns / 20))
            min 20 (max 10 (round (toFloat w / toFloat columns / 20) - columns))

        --22 - columns
        --round (toFloat w / toFloat columns / 20 + 3)
    in
    case exp of
        Just exposition ->
            column
                [ --Border.color (rgb255 0 0 0)
                  --, Border.width 2
                  --, Border.rounded 3
                  Element.centerX
                , spacing 10
                , paddingXY 5 25
                ]
                [ paragraph
                    [ height fill
                    , Font.center
                    , Font.size fontSize
                    , Font.bold
                    ]
                    [ Element.newTabLink
                        []
                        { url = exposition.url
                        , label = Element.text exposition.title
                        }
                    ]
                , paragraph
                    [ Element.centerX
                    , Font.center
                    , Font.size (fontSize - 1)
                    , Element.paddingEach { defaultPadding | bottom = 24 }
                    ]
                    (authorLinks exposition.author exposition.coauthors)
                ]

        Nothing ->
            Element.text "Waiting for exposition..."


viewTitleAuthorAbstract : Int -> Int -> Maybe Exposition -> Element Msg
viewTitleAuthorAbstract w columns exp =
    case exp of
        Just exposition ->
            let
                title =
                    String.length exposition.title

                author =
                    String.length exposition.author.name

                chars =
                    title + author

                shortAbstract =
                    let
                        factor =
                            logBase 1.8 (toFloat w / toFloat columns)
                    in
                    if columns > 2 then
                        String.Extra.softEllipsis (round (factor * 50) - chars) exposition.abstract
                        --String.Extra.softEllipsis (round (toFloat w / (7 / toFloat columns * 4) * 2) - chars) exposition.abstract

                    else
                        String.Extra.softEllipsis (round (toFloat w / 3) - chars) exposition.abstract

                cols =
                    if w > 675 then
                        max 2 columns

                    else
                        columns

                fontSize =
                    min 20 (max 12 (round (toFloat w / toFloat cols / 20) - columns))

                --max 16 (round (toFloat w / (toFloat columns / 2) / 20))
                --22 - columns
                --round (toFloat w / toFloat columns / 20)
            in
            column
                [ Element.centerX
                , spacing 10
                , padding (round (toFloat w / 10 / toFloat columns))
                ]
                [ paragraph
                    [ height fill
                    , Font.center
                    , Font.size fontSize

                    --, Font.size (22 - columns)
                    , Font.bold
                    ]
                    [ Element.newTabLink
                        []
                        { url = exposition.url
                        , label = Element.text exposition.title
                        }
                    ]
                , paragraph
                    [ Element.centerX
                    , Font.center
                    , Font.size (fontSize - 1)
                    , Element.paddingEach { defaultPadding | bottom = 24 }
                    ]
                    (authorLinks exposition.author exposition.coauthors)
                , paragraph
                    [ --, Background.color (rgb255 160 250 100)
                      Element.centerX
                    , Font.size (fontSize - 2)

                    --, Font.size 15
                    ]
                    [ Element.text shortAbstract ]
                ]

        Nothing ->
            Element.text "Waiting for exposition..."


viewMobile : Int -> Int -> Maybe Exposition -> Element Msg
viewMobile w columns exp =
    case exp of
        Just exposition ->
            let
                title =
                    String.length exposition.title

                author =
                    String.length exposition.author.name

                chars =
                    title + author

                shortAbstract =
                    let
                        factor =
                            logBase 1.9 (toFloat w)
                    in
                    String.Extra.softEllipsis (round factor * 50 - chars) exposition.abstract

                cols =
                    if w > 675 then
                        max 2 columns

                    else
                        columns

                fontSize =
                    min 20 (max 12 (round (toFloat w / toFloat cols / 20) - columns))

                --max 16 (round (toFloat w / (toFloat columns / 2) / 20))
                --22 - columns
                --round (toFloat w / toFloat columns / 20)
            in
            column
                [ Element.centerX
                , spacing 10
                , padding (round (toFloat w / 10 / toFloat columns))
                ]
                [ paragraph
                    [ height fill
                    , Font.center
                    , Font.size fontSize

                    --, Font.size (22 - columns)
                    , Font.bold
                    ]
                    [ Element.newTabLink
                        []
                        { url = exposition.url
                        , label = Element.text exposition.title
                        }
                    ]
                , paragraph
                    [ --Background.color (rgb255 0 250 160)
                      Element.centerX
                    , Font.center
                    , Font.size (fontSize - 1)

                    --, Font.size (20 - columns)
                    , Element.paddingEach { defaultPadding | bottom = 24 }
                    ]
                    [ Element.newTabLink
                        []
                        { url = authorLink exposition.author.id
                        , label = Element.text exposition.author.name
                        }
                    ]
                , paragraph
                    [ --, Background.color (rgb255 160 250 100)
                      Element.centerX
                    , Font.size (fontSize - 2)

                    --, Font.size 15
                    ]
                    [ Element.text shortAbstract ]
                ]

        Nothing ->
            Element.text "Waiting for exposition..."


viewExpositionColumn : Int -> Int -> Maybe Exposition -> Element Msg
viewExpositionColumn w columns exp =
    case exp of
        Just exposition ->
            let
                expositionPath =
                    defaultPageFromUrl exposition.url

                image =
                    case Maybe.withDefault "" exposition.thumb of
                        "" ->
                            "https://keywords.sarconference2016.net/screenshots2" ++ expositionPath ++ "/0.png"

                        _ ->
                            Maybe.withDefault "" exposition.thumb

                --amountOfText =
                --String.length shortAbstract + String.length exposition.title
                -- "smart" scaling
                imgHeight =
                    round (toFloat w / toFloat (columns + 1))

                imageHeight =
                    px imgHeight

                --px (round (toFloat w / 4))
                --px (200 + (500 // amountOfText * 20))
            in
            Element.column
                [ width fill

                --, Border.color (rgb255 0 0 0)
                --, Border.width 2
                --, Border.rounded 3
                , Element.alignTop

                --, Element.clip
                --, Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
                ]
                [ el
                    [ --Background.color (rgb255 0 250 160)
                      Element.centerX
                    ]
                    (Element.newTabLink
                        []
                        { url = exposition.url --image
                        , label =
                            Element.image
                                [ width fill
                                , height imageHeight
                                ]
                                { src = image
                                , description = "" --image ++ " preview image of the exposition"
                                }
                        }
                    )
                , if w > 674 then
                    if columns > 6 then
                        Element.none

                    else if imgHeight > 250 then
                        viewTitleAuthorAbstract w columns exp

                    else if imgHeight < 150 then
                        viewTitle w columns exp

                    else
                        viewTitleAuthor w columns exp

                  else
                    viewMobile w columns exp
                ]

        Nothing ->
            Element.text "Loading..."


viewExpositionWide : Int -> Int -> Maybe Exposition -> Element Msg
viewExpositionWide w columns exp =
    case exp of
        Just exposition ->
            let
                expositionPath =
                    defaultPageFromUrl exposition.url

                image =
                    case Maybe.withDefault "" exposition.thumb of
                        "" ->
                            "https://keywords.sarconference2016.net/screenshots2" ++ expositionPath ++ "/0.png"

                        _ ->
                            Maybe.withDefault "" exposition.thumb

                --amountOfText =
                --String.length shortAbstract + String.length exposition.title
                -- "smart" scaling
                imgHeight =
                    round (toFloat w / toFloat (columns + 1))

                _ =
                    Debug.log "imgHeight" imgHeight

                imageHeight =
                    px imgHeight

                --px (round (toFloat w / 4))
                --px (200 + (500 // amountOfText * 20))
            in
            Element.column
                [ width fill

                --, Border.color (rgb255 0 0 0)
                --, Border.width 2
                --, Border.rounded 3
                , Element.alignTop

                --, Element.clip
                --, Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
                ]
                [ el
                    [ --Background.color (rgb255 0 250 160)
                      Element.centerX
                    ]
                    (Element.newTabLink
                        []
                        { url = exposition.url --image
                        , label =
                            Element.image
                                [ width fill
                                , height imageHeight
                                ]
                                { src = image
                                , description = "" --image ++ " preview image of the exposition"
                                }
                        }
                    )
                , if imgHeight < 150 then
                    Element.none

                  else if imgHeight < 175 then
                    viewTitle w columns exp

                  else if imgHeight > 250 then
                    viewTitleAuthorAbstract w columns exp

                  else
                    viewTitleAuthor w columns exp
                ]

        Nothing ->
            Element.text "Loading..."


main : Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = ChangeUrl
        , onUrlRequest = RequestUrl
        }


subscriptions : a -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowWasResized


authorLink : Int -> String
authorLink authorId =
    "https://www.researchcatalogue.net/profile/?person=" ++ String.fromInt authorId
