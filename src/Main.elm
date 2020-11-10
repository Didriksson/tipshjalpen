module Main exposing (..)

import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Attribute, Html, button, div, header, img, input, li, p, span, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (request)
import Json.Decode as D exposing (int)
import Maybe exposing (Maybe)
import Regex



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Model
    = Loading
    | Success PageState
    | Failure


type alias Score =
    { hemmalag : Int
    , bortalag : Int
    }


type alias Analys =
    { predictedScore : Score
    , outcomePercentage : MatchInfoHallare
    }


type alias KupongRad =
    { home : String
    , away : String
    , liga : String
    , svenskaFolket : MatchInfoHallare
    , odds : MatchInfoHallare
    , analys : Maybe Analys
    }


type alias Kupong =
    { name : String
    , rader : List KupongRad
    }


type alias PageState =
    { kupong : Kupong
    , rad : Maybe KupongRad
    }


type alias MatchInfoHallare =
    { hemmalag : String
    , kryss : String
    , bortalag : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "https://tipshjalpen.herokuapp.com/hamtaKupong"
        , expect = Http.expectJson GotOppenKupong kupongDecoder
        }
    )



-- UPDATE


type Msg
    = GotOppenKupong (Result Http.Error Kupong)
    | KlickadRad KupongRad


{-| Returnerar det första värdet i listan som inte är Nothing.
Om alla värden är nothing returneras Nothing.
-}
maybeOneOf : List (Maybe a) -> Maybe a
maybeOneOf maybes =
    maybes
        |> List.filterMap identity
        |> List.head


{-| Validerar att en inmatat gardering är ett giltigt värde.
Giltiga värden är heltal mellan 1 och 99.
-}
valideraGarderingInput : String -> Maybe Int
valideraGarderingInput input =
    case String.toInt input of
        Nothing ->
            Nothing

        Just n ->
            if n < 100 && n >= 0 then
                Just n

            else
                Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KlickadRad klickadRad ->
            case model of
                Success state ->
                    ( Success { state | rad = Just klickadRad }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotOppenKupong result ->
            case model of
                Loading ->
                    case result of
                        Ok received ->
                            ( Success (PageState received Nothing), Cmd.none )

                        Err _ ->
                            ( Failure, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


header : Element msg
header =
    el
        [ height (px 100)
        , width fill
        , Background.color <| rgb255 150 99 118
        ]
    <|
        el
            [ centerX
            , centerY
            , Font.color <| rgb255 255 255 255
            , Font.size 50
            ]
            (text "Tipshjälpen")


kupongView : Kupong -> Element Msg
kupongView kupong =
    column
        [ width fill
        , Background.color <| rgb255 250 75 75
        , paddingEach { top = 20, left = 5, right = 5, bottom = 20 }
        , Border.rounded 10
        , spacing 10
        ]
    <|
        el [] (text kupong.name)
            :: List.map kupongRadView kupong.rader


kupongRadView : KupongRad -> Element Msg
kupongRadView rad =
    column
        [ Element.Events.onClick (KlickadRad rad)
        , width fill
        , Border.widthEach { bottom = 0, top = 2, left = 0, right = 0 }
        , Border.color <| rgba255 192 192 192 0.3
        ]
        [ el [ width fill, Font.size 10 ] (text rad.liga)
        , row [ width fill ]
            [ column [ height fill, width <| fillPortion 3 ]
                [ el [ Element.alignTop ] (text rad.home)
                , el [ Element.alignBottom ] (text rad.away)
                ]
            , row [ height fill, spacing 10, Font.size 12 ]
                [ column [ height fill ]
                    [ el [] (text (rad.svenskaFolket.hemmalag ++ "%"))
                    , el [] (text (rad.svenskaFolket.kryss ++ "%"))
                    , el [] (text (rad.svenskaFolket.bortalag ++ "%"))
                    ]
                , column [ height fill ]
                    [ el [] (text rad.odds.hemmalag)
                    , el [] (text rad.odds.kryss)
                    , el [] (text rad.odds.bortalag)
                    ]
                ]
            , column [ height fill, width <| fillPortion 3 ] <|
                predictedScoreView rad.analys
            ]
        ]


predictedScoreView : Maybe Analys -> List (Element msg)
predictedScoreView maybeAnalys =
    case maybeAnalys of
        Just a ->
            [ el [ Element.alignRight, Element.alignTop ] (text (String.fromInt a.predictedScore.hemmalag))
            , el [ Element.alignRight, Element.alignBottom ] (text (String.fromInt a.predictedScore.bortalag))
            ]

        Nothing ->
            [ el [] Element.none ]


mainView : PageState -> Element Msg
mainView state =
    row [ Background.color <| rgba255 100 100 100 0.5, width fill, height fill, padding 50, spacing 20 ]
        [ column
            [ width fill
            , centerY
            , spacing 10
            , Border.color <| rgb255 0xE0 0xE0 0xE0
            ]
            [ kupongView state.kupong
            ]
        , column [ width fill, height fill, spacing 10 ]
            [ case state.rad of
                Nothing ->
                    Element.none

                Just rad ->
                    analyzeView rad
            ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            layout [] <|
                column [ height fill, width fill ]
                    [ header
                    , el
                        [ centerX
                        , centerY
                        , Font.color <| rgb255 255 255 255
                        , Font.size 50
                        ]
                        (text "Laddar in kupong...")
                    ]

        Success results ->
            layout [] <|
                column [ height fill, width fill ]
                    [ header
                    , mainView results
                    ]

        Failure ->
            layout [] <| text "Det gick inte så bra att hitta en kupong."


analyzeView : KupongRad -> Element Msg
analyzeView rad =
    case rad.analys of
        Just analys ->
            column [ width fill, height fill, spacing 10, padding 10, Border.rounded 10, Background.color <| rgb255 51 255 128 ]
                [ column [ centerX, Font.bold ]
                    [ el [ centerX, Font.extraLight ] <| text rad.liga
                    , el [ centerX ] <| text (rad.home ++ " - " ++ rad.away)
                    ]
                , column []
                    [ el [] <| text ("Predicted score: " ++ String.fromInt analys.predictedScore.hemmalag ++ " - " ++ String.fromInt analys.predictedScore.bortalag)
                    , el [] <| text ("Poissonanalys win/draw/win: " ++ analys.outcomePercentage.hemmalag ++ " " ++ analys.outcomePercentage.kryss ++ " " ++ analys.outcomePercentage.bortalag)
                    , el [] <| text ("Odds" ++ " 1: " ++ rad.odds.hemmalag ++ " X: " ++ rad.odds.kryss ++ " 2: " ++ rad.odds.bortalag)
                    , el [] <| text ("Svenska folket: " ++ rad.svenskaFolket.hemmalag ++ "%" ++ " " ++ rad.svenskaFolket.kryss ++ "%" ++ " " ++ rad.svenskaFolket.bortalag ++ "%")
                    ]
                ]

        Nothing ->
            text "Kunde inte analysera den här matchen. Försök igen senare."


analysDecoder : D.Decoder Analys
analysDecoder =
    D.map2
        Analys
        (D.field "predictedScore"
            (D.map2 Score
                (D.field "hemmalag" D.int)
                (D.field "bortalag" D.int)
            )
        )
        (D.field "outcomePercentage" matchInfoHallareDecoder)


matchInfoHallareDecoder : D.Decoder MatchInfoHallare
matchInfoHallareDecoder =
    D.map3 MatchInfoHallare
        (D.field "hemmalag" D.string)
        (D.field "kryss" D.string)
        (D.field "bortalag" D.string)


kupongRadDecoder : D.Decoder KupongRad
kupongRadDecoder =
    D.map6 KupongRad
        (D.field "hemmalag" D.string)
        (D.field "bortalag" D.string)
        (D.field "liga" D.string)
        (D.field "svenskaFolket" matchInfoHallareDecoder)
        (D.field "odds" matchInfoHallareDecoder)
        (D.maybe (D.field "analys" analysDecoder))


kupongDecoder : D.Decoder Kupong
kupongDecoder =
    D.map2
        Kupong
        (D.field "namn" D.string)
        (D.field "matcher" (D.list kupongRadDecoder))
