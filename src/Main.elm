module Main exposing (..)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Array2D.Json as GridDecoder exposing (decoder)
import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font exposing (alignLeft, center)
import Element.Input as Input
import Html exposing (Attribute, Html, b, button, div, header, img, input, li, p, span, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (request)
import Json.Decode as D exposing (int)
import Json.Encode as Encode
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        , render
        )
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


type alias Poissonanalys =
    { predictedScore : Score
    , outcomePercentage : MatchInfoHallare
    , poissonTable : List (List Float)
    }


type alias Analys =
    { poisson : Maybe Poissonanalys
    , radforslag : Maybe MatchInfoHallare
    }


type alias KupongRad =
    { home : String
    , away : String
    , liga : String
    , svenskaFolket : MatchInfoHallare
    , odds : Maybe MatchInfoHallare
    , analys : Maybe Analys
    }


type alias Kupong =
    { name : String
    , rader : List KupongRad
    }


type alias Forslag =
    { nr : String
    , rad : String
    }


type alias PageState =
    { kupong : Kupong
    , rad : Maybe KupongRad
    , radforslag : Dict Int String
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
    | SystemforslagChanged String Int Bool
    | SkickaMedUppdateratRadforslag PageState


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
        SkickaMedUppdateratRadforslag state ->
            ( Loading
            , Http.post
                { -- url = "https://tipshjalpen.herokuapp.com/hamtaKupong"
                  url = "http://localhost:5000/hamtaKupong"
                , body = Http.jsonBody <| forslagEncoder state.radforslag
                , expect = Http.expectJson GotOppenKupong kupongDecoder
                }
            )

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
                            ( Success (PageState received Nothing Dict.empty), Cmd.none )

                        Err e ->
                            Debug.log (Debug.toString e)
                                ( Failure, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SystemforslagChanged tecken match b ->
            case model of
                Success state ->
                    let
                        uppdateradRad =
                            case Dict.get match state.radforslag of
                                Just ursprunligt ->
                                    case b of
                                        False ->
                                            String.replace tecken "" ursprunligt

                                        True ->
                                            ursprunligt ++ tecken

                                Nothing ->
                                    case b of
                                        True ->
                                            tecken

                                        _ ->
                                            ""
                    in
                    ( Success
                        { state
                            | radforslag =
                                if String.isEmpty uppdateradRad then
                                    Dict.remove match state.radforslag

                                else
                                    Dict.insert match uppdateradRad state.radforslag
                        }
                    , Cmd.none
                    )

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


kupongView : PageState -> Element Msg
kupongView state =
    column
        [ width fill
        , Background.color <| rgb255 250 250 250
        , paddingEach { top = 20, left = 5, right = 5, bottom = 20 }
        , Border.rounded 10
        ]
    <|
        row [ width fill ]
            [ el [ padding 10, width fill ] (text state.kupong.name)
            , Input.button
                [ Element.alignRight
                , Background.color <| rgb255 255 255 255
                , Element.focused
                    [ Background.color <| rgb255 192 192 192 ]
                ]
                { onPress = Just (SkickaMedUppdateratRadforslag state)
                , label = el [ Border.rounded 16 ] (text "Uppdatera")
                }
            ]
            :: List.indexedMap
                (\ind rad ->
                    kupongRadView ind rad state.radforslag
                )
                state.kupong.rader


kupongRadView : Int -> KupongRad -> Dict Int String -> Element Msg
kupongRadView index rad radforslag =
    row
        [ width fill
        , Border.widthEach { bottom = 0, top = 2, left = 0, right = 0 }
        , Border.color <| rgba255 192 192 192 0.3
        ]
        [ Input.button
            [ width fill
            , Background.color <| rgb255 255 255 255
            , Element.focused
                [ Background.color <| rgb255 192 192 192 ]
            ]
            { onPress = Just (KlickadRad rad)
            , label = matchinfoView index rad
            }
        , el [ height fill, width shrink, alignRight, centerY ] (systemradRowView index rad (Dict.get index radforslag))
        ]


matchinfoView : Int -> KupongRad -> Element Msg
matchinfoView index rad =
    row [ width fill ]
        [ el [ Font.size 10 ] (text (String.fromInt (index + 1) ++ "."))
        , column [ width fill, padding 10 ]
            [ el [ width fill, Font.size 10 ] (text rad.liga)
            , row [ width fill ]
                [ column [ height fill, width <| fillPortion 3 ]
                    [ el [ Element.alignTop ] (text rad.home)
                    , el [ Element.alignBottom ] (text rad.away)
                    ]
                , row [ width fill, height fill, spacing 10, Font.size 12 ]
                    [ column [ height fill ]
                        [ el [] (text (rad.svenskaFolket.hemmalag ++ "%"))
                        , el [] (text (rad.svenskaFolket.kryss ++ "%"))
                        , el [] (text (rad.svenskaFolket.bortalag ++ "%"))
                        ]
                    , column [ height fill ] <|
                        oddsOrNothingView rad.odds
                    ]
                , column [ alignRight, height fill, width <| fillPortion 3 ] <|
                    predictedScoreView rad.analys
                ]
            ]
        ]


oddsOrNothingView : Maybe MatchInfoHallare -> List (Element msg)
oddsOrNothingView maybeOdds =
    case maybeOdds of
        Just odds ->
            [ el [] (text odds.hemmalag)
            , el [] (text odds.kryss)
            , el [] (text odds.bortalag)
            ]

        Nothing ->
            [ Element.none ]


predictedScoreView : Maybe Analys -> List (Element msg)
predictedScoreView maybeAnalys =
    case maybeAnalys of
        Just a ->
            [ el [ Element.alignRight, Element.alignTop ] (text (String.fromInt a.predictedScore.hemmalag))
            , el [ Element.alignRight, Element.alignBottom ] (text (String.fromInt a.predictedScore.bortalag))
            ]

        Nothing ->
            [ Input.button
                [ Element.alignRight
                , Font.color <| rgb255 252 3 73
                , Font.extraBold
                , Font.size 35
                , mouseOver
                    [ Background.color <| rgb255 0xFF 0xFF 0xFF, Font.color <| rgb255 0 0 0 ]
                , focused
                    [ Border.shadow { offset = ( 4, 4 ), size = 3, blur = 10, color = rgb255 114 159 207 } ]
                ]
                { onPress = Nothing
                , label = el [] (text "!")
                }
            ]


mainView : PageState -> Element Msg
mainView state =
    row [ Background.color <| rgba255 100 100 100 0.5, width fill, height fill, padding 50, spacing 20 ]
        [ column
            [ width fill
            , centerY
            , spacing 10
            , Border.color <| rgb255 0xE0 0xE0 0xE0
            , alignTop
            ]
            [ kupongView state
            ]
        , column [ width fill, height fill, spacing 10 ]
            [ case state.rad of
                Nothing ->
                    Element.none

                Just rad ->
                    analyzeView rad
            ]
        ]


emptysystemRad : Int -> List (Element Msg)
emptysystemRad matchnummer =
    [ el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Etta") "1" matchnummer False
    , el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Kryss") "x" matchnummer False
    , el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Tvåa") "2" matchnummer False
    ]


systemradRowView : Int -> KupongRad -> Maybe String -> Element Msg
systemradRowView matchnummer rad forslag =
    row
        [ Border.color <| rgb255 0xC0 0xC0 0xC0
        , Border.widthEach { bottom = 0, top = 0, left = 2, right = 0 }
        , centerX
        , center
        , width fill
        , height fill
        , spacing 10
        , paddingXY 10 0
        ]
    <|
        case forslag of
            Just f ->
                [ el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Etta") "1" matchnummer (String.contains "1" f)
                , el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Kryss") "x" matchnummer (String.contains "x" f)
                , el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Tvåa") "2" matchnummer (String.contains "2" f)
                ]

            Nothing ->
                case rad.analys of
                    Just analys ->
                        case analys.radforslag of
                            Just radforslag ->
                                [ el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Etta") "1" matchnummer (radforslag.hemmalag == "True")
                                , el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Kryss") "x" matchnummer (radforslag.kryss == "True")
                                , el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Tvåa") "2" matchnummer (radforslag.bortalag == "True")
                                ]

                            Nothing ->
                                emptysystemRad matchnummer

                    Nothing ->
                        emptysystemRad matchnummer


checkboxInput : String -> String -> Int -> Bool -> Element Msg
checkboxInput labelText icontext matchnummer kryssad =
    Input.checkbox [ centerX, width fill, height fill ]
        { onChange = SystemforslagChanged icontext matchnummer
        , icon = checkboxIcon icontext
        , checked = kryssad
        , label = Input.labelHidden labelText
        }


checkboxIcon : String -> Bool -> Element msg
checkboxIcon labeltext isChecked =
    el
        [ width <| px 30
        , height <| px 30
        , centerY
        , centerX
        , padding 4
        , Border.rounded 6
        , Border.width 2
        , Border.color <| rgb255 0xC0 0xC0 0xC0
        ]
    <|
        el
            [ width fill
            , height fill
            , Border.rounded 4
            , Background.color <|
                if isChecked then
                    rgb255 114 159 207

                else
                    rgb255 0xFF 0xFF 0xFF
            ]
        <|
            text labeltext


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            layout [] <|
                column [ height fill, width fill ]
                    [ header
                    , row [ centerX, centerY ]
                        [ el
                            [ centerX
                            , centerY
                            , Font.color <| rgb255 0 0 0
                            , Font.size 50
                            ]
                            (text "Laddar in kupong")
                        , el [ alignBottom ] <|
                            Element.html
                                (Loading.render
                                    BouncingBalls
                                    { defaultConfig | color = "#333" }
                                    Loading.On
                                )
                        ]
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
            column [ width fill, height fill, padding 10, Border.rounded 10, Background.color <| rgb255 51 255 128 ]
                [ column [ centerX, Font.bold ]
                    [ el [ centerX, Font.extraLight ] <| text rad.liga
                    , el [ centerX ] <| text (rad.home ++ " - " ++ rad.away)
                    ]
                , column []
                    [ el [] <| text ("Predicted score: " ++ String.fromInt analys.predictedScore.hemmalag ++ " - " ++ String.fromInt analys.predictedScore.bortalag)
                    , el [] <| text ("Poissonanalys win/draw/win: " ++ analys.outcomePercentage.hemmalag ++ " " ++ analys.outcomePercentage.kryss ++ " " ++ analys.outcomePercentage.bortalag)
                    , case rad.odds of
                        Just o ->
                            el [] <| text ("Odds" ++ " 1: " ++ o.hemmalag ++ " X: " ++ o.kryss ++ " 2: " ++ o.bortalag)

                        Nothing ->
                            Element.none
                    , el [] <| text ("Svenska folket: " ++ rad.svenskaFolket.hemmalag ++ "%" ++ " " ++ rad.svenskaFolket.kryss ++ "%" ++ " " ++ rad.svenskaFolket.bortalag ++ "%")
                    ]
                , column [ width fill, height (px 300), Font.center ] <|
                    headerRow 5
                        :: List.take 6 (List.indexedMap poissonTableRowView analys.poissonTable)
                ]

        Nothing ->
            text "Kunde inte analysera den här matchen. Försök igen senare."


headerRow : Int -> Element msg
headerRow goals =
    row [ width fill, Background.color <| rgb255 160 150 250, height fill ] <|
        el [ width fill ] (el [ width fill, centerX, centerY ] <| text "Goals")
            :: List.map
                (\n ->
                    el [ width fill, centerX, centerY, Font.center ] <|
                        text (String.fromInt n)
                )
                (List.range 0 goals)


poissonTableRowView : Int -> List Float -> Element msg
poissonTableRowView index r =
    row [ width fill, height fill ] <|
        el [ width fill, height fill, Background.color <| rgb255 160 150 250 ] (el [ width fill ] (text (String.fromInt index)))
            :: List.take 6 (List.indexedMap (poissonTableColView index) r)


poissonTableColView : Int -> Int -> Float -> Element msg
poissonTableColView rowIndex colIndex col =
    let
        color =
            if rowIndex > colIndex then
                rgb255 0 255 0

            else if rowIndex == colIndex then
                rgb255 255 255 255

            else
                rgb255 255 0 0
    in
    el [ width fill, height fill, Background.color color ] <|
        el [ centerY, centerX ] <|
            text (String.fromFloat col ++ "%")



--- Tyvärr så måste jag köra en magisk "+1 på matcherna här eftersom det känns lite orimligt att skicka det med start på match 0..."


encodaForslagslista : Dict Int String -> List (List ( String, Encode.Value ))
encodaForslagslista forslag =
    List.map
        (\data ->
            [ ( "nr", Encode.int (Tuple.first data + 1) )
            , ( "forslag", Encode.string (Tuple.second data) )
            ]
        )
        (Dict.toList forslag)


forslagEncoder : Dict Int String -> Encode.Value
forslagEncoder forslag =
    Encode.object
        [ ( "rad"
          , Encode.list Encode.object
                (encodaForslagslista forslag)
          )
        ]


analysDecoder : D.Decoder Analys
analysDecoder =
    D.map4
        Analys
        (D.field "poisson"
            (D.field "predictedScore"
                (D.map2 Score
                    (D.field "hemmalag" D.int)
                    (D.field "bortalag" D.int)
                )
            )
        )
        (D.field "poisson" (D.field "outcomePercentage" matchInfoHallareDecoder))
        (D.field "poisson" (D.field "poissonTable" (D.list (D.list D.float))))
        (optionalField "radforslag" matchInfoHallareDecoder)


matchInfoHallareDecoder : D.Decoder MatchInfoHallare
matchInfoHallareDecoder =
    D.map3
        MatchInfoHallare
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
        (optionalField "odds" matchInfoHallareDecoder)
        (optionalField "analys" analysDecoder)


kupongDecoder : D.Decoder Kupong
kupongDecoder =
    D.map2
        Kupong
        (D.field "namn" D.string)
        (D.field "matcher" (D.list kupongRadDecoder))


optionalField : String -> D.Decoder a -> D.Decoder (Maybe a)
optionalField fieldName decoder =
    D.value
        |> D.andThen
            (\value ->
                case D.decodeValue (D.field fieldName D.value) value of
                    Ok fieldValue ->
                        case D.decodeValue decoder fieldValue of
                            Ok a ->
                                D.succeed (Just a)

                            Err errorMessage ->
                                D.fail (D.errorToString errorMessage)

                    Err err ->
                        D.succeed Nothing
            )
