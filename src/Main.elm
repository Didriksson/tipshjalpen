module Main exposing (..)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Array2D.Json as GridDecoder exposing (decoder)
import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (color)
import Element.Events exposing (..)
import Element.Font as Font exposing (alignLeft, center)
import Element.Input as Input
import Html exposing (Attribute, Html, a, b, button, div, header, img, input, label, li, output, p, span, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (request)
import Json.Decode as D exposing (decodeString, int)
import Json.Encode as Encode
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        , render
        )
import Maybe exposing (Maybe, withDefault)
import Regex exposing (Match)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Model
    = Selection
    | LoadingTips Radforslag
    | LoadingOverunder
    | SuccessTips TipsState
    | SuccessOverUnder OverUnder
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
    , radforslag : Maybe MatchInfoHallareBool
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


type alias Match =
    { hemmalag : String
    , bortalag : String
    , liga : String
    , resultat : Maybe Score
    , date : String
    }


type alias OverUnderItem =
    { match : Match
    , poisson : List String
    , senasteHemma : List Match
    , senasteBorta : List Match
    }


type alias OverUnder =
    { data : List OverUnderItem
    , overUnderModal : OverUnderModalState
    }


type OverUnderModalState
    = NoneModal
    | HemmalagModal Match
    | BortalagModal Match


type alias Forslag =
    { nr : String
    , rad : String
    }


type alias Radforslag =
    Dict Int MatchInfoHallareBool


type alias TipsState =
    { kupong : Kupong
    , rad : Maybe KupongRad
    , grundrad : Radforslag
    }


type alias MatchInfoHallare =
    { hemmalag : String
    , kryss : String
    , bortalag : String
    }


type alias MatchInfoHallareBool =
    { hemmalag : Bool
    , kryss : Bool
    , bortalag : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Selection
    , Cmd.none
    )



-- UPDATE


type Msg
    = HeaderClick
    | SelectedTipset
    | SelectedOverUnder
    | GotOppenKupong (Result Http.Error Kupong)
    | GotOverunder (Result Http.Error OverUnder)
    | KlickadRad KupongRad
    | SystemforslagChanged String Int Bool
    | SkickaMedUppdateratRadforslag TipsState
    | OverUnderVisaModal OverUnderModalState


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


uppdateraRadforslag : MatchInfoHallareBool -> String -> Bool -> MatchInfoHallareBool
uppdateraRadforslag rad tecken nyttvarde =
    case tecken of
        "1" ->
            MatchInfoHallareBool nyttvarde rad.kryss rad.bortalag

        "x" ->
            MatchInfoHallareBool rad.hemmalag nyttvarde rad.bortalag

        "2" ->
            MatchInfoHallareBool rad.hemmalag rad.kryss nyttvarde

        _ ->
            rad


matchInfoHallareBoolIsAllEmpty : MatchInfoHallareBool -> Bool
matchInfoHallareBoolIsAllEmpty matchinfo =
    not matchinfo.hemmalag && not matchinfo.kryss && not matchinfo.bortalag


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OverUnderVisaModal modal ->
            case model of
                SuccessOverUnder state ->
                    ( SuccessOverUnder { state | overUnderModal = modal }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        HeaderClick ->
            ( Selection, Cmd.none )

        SelectedTipset ->
            ( LoadingTips Dict.empty
            , Http.get
                { url = "http://localhost:5000/hamtaKupong"
                , expect = Http.expectJson GotOppenKupong kupongDecoder
                }
            )

        SelectedOverUnder ->
            ( LoadingOverunder
            , Http.get
                { url = "http://localhost:5000/overunder"
                , expect = Http.expectJson GotOverunder overUnderDecoder
                }
            )

        SkickaMedUppdateratRadforslag state ->
            ( LoadingTips state.grundrad
            , Http.post
                { -- url = "http://localhost:5000/hamtaKupong"
                  url = "http://localhost:5000/hamtaKupong"
                , body = Http.jsonBody <| forslagEncoder state.grundrad
                , expect = Http.expectJson GotOppenKupong kupongDecoder
                }
            )

        KlickadRad klickadRad ->
            case model of
                SuccessTips state ->
                    ( SuccessTips { state | rad = Just klickadRad }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotOverunder result ->
            case model of
                LoadingOverunder ->
                    case result of
                        Ok received ->
                            ( SuccessOverUnder received, Cmd.none )

                        Err e ->
                            Debug.log (Debug.toString e)
                                ( Failure, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotOppenKupong result ->
            case model of
                LoadingTips grundrad ->
                    case result of
                        Ok received ->
                            ( SuccessTips (TipsState received Nothing grundrad), Cmd.none )

                        Err e ->
                            Debug.log (Debug.toString e)
                                ( Failure, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SystemforslagChanged tecken match b ->
            case model of
                SuccessTips state ->
                    let
                        uppdaterad =
                            Dict.get match state.grundrad
                                |> Maybe.withDefault (MatchInfoHallareBool False False False)
                                |> (\rad -> uppdateraRadforslag rad tecken b)
                    in
                    ( SuccessTips
                        { state
                            | grundrad =
                                if matchInfoHallareBoolIsAllEmpty uppdaterad then
                                    Dict.remove match state.grundrad

                                else
                                    Dict.insert match uppdaterad state.grundrad
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


header : Element Msg
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
            (Input.button []
                { label = text "Tipshjälpen"
                , onPress = Just HeaderClick
                }
            )


kupongView : TipsState -> Element Msg
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
                    kupongRadView ind rad state.grundrad
                )
                state.kupong.rader


kupongRadView : Int -> KupongRad -> Radforslag -> Element Msg
kupongRadView index rad grundrad =
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
        , el [ height fill, width shrink, alignRight, centerY ] (systemradRowView index rad (Dict.get index grundrad))
        ]


matchinfoView : Int -> KupongRad -> Element Msg
matchinfoView index rad =
    row [ width fill ]
        [ el [ Font.size 10 ] (text (String.fromInt (index + 1) ++ "."))
        , column [ width fill, padding 10 ]
            [ el [ width fill, Font.size 10 ] (text rad.liga)
            , row [ width fill ]
                [ column [ height fill, width fill ]
                    [ el [ Element.alignTop ] (text rad.home)
                    , el [ Element.alignBottom ] (text rad.away)
                    ]
                , row [ height fill, Font.size 12, width <| px 100 ]
                    [ column [ height fill, width fill ]
                        [ el [] (text (rad.svenskaFolket.hemmalag ++ "%"))
                        , el [] (text (rad.svenskaFolket.kryss ++ "%"))
                        , el [] (text (rad.svenskaFolket.bortalag ++ "%"))
                        ]
                    , column [ height fill, width fill ] <|
                        oddsOrNothingView rad.odds
                    ]
                , column [ width <| px 30, alignRight, height fill ] <|
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


matchInfoHallareBoolOrEmpty : Maybe MatchInfoHallareBool -> MatchInfoHallareBool
matchInfoHallareBoolOrEmpty maybeMatchinfo =
    Maybe.withDefault (MatchInfoHallareBool False False False) maybeMatchinfo


predictedScoreView : Maybe Analys -> List (Element msg)
predictedScoreView maybeAnalys =
    let
        predicted =
            maybeAnalys |> Maybe.andThen (\v -> v.poisson)
    in
    case predicted of
        Just a ->
            [ el [ Element.alignRight, Element.alignTop ] (text (String.fromInt a.predictedScore.hemmalag))
            , el [ Element.alignRight, Element.alignBottom ] (text (String.fromInt a.predictedScore.bortalag))
            ]

        Nothing ->
            [ Input.button
                [ Element.alignRight
                , Font.color <| rgb255 252 3 73
                , Font.extraBold
                , Font.size 30
                , mouseOver
                    [ Background.color <| rgb255 0xFF 0xFF 0xFF, Font.color <| rgb255 0 0 0 ]
                , focused
                    [ Border.shadow { offset = ( 4, 4 ), size = 3, blur = 10, color = rgb255 114 159 207 } ]
                , htmlAttribute (Html.Attributes.title "Kunde inte analysera match. Om du vill ha ett radförslag av systemet behöver du inkludera denna i en grundrad och uppdatera.")
                ]
                { onPress = Nothing
                , label = el [] (text "!")
                }
            ]


mainTipsView : TipsState -> Element Msg
mainTipsView state =
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


systemradRowView : Int -> KupongRad -> Maybe MatchInfoHallareBool -> Element Msg
systemradRowView matchnummer rad maybe_grundrad =
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
        let
            radforslag =
                rad.analys
                    |> Maybe.andThen (\a -> a.radforslag)
                    |> matchInfoHallareBoolOrEmpty

            grundrad =
                matchInfoHallareBoolOrEmpty maybe_grundrad
        in
        [ el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Etta") "1" matchnummer radforslag.hemmalag grundrad.hemmalag
        , el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Kryss") "x" matchnummer radforslag.kryss grundrad.kryss
        , el [ centerX, height fill ] <| checkboxInput ("Match " ++ String.fromInt matchnummer ++ " - Tvåa") "2" matchnummer radforslag.bortalag grundrad.bortalag
        ]


checkboxInput : String -> String -> Int -> Bool -> Bool -> Element Msg
checkboxInput labelText icontext matchnummer forslag anvandarforslag =
    Input.checkbox [ centerX, width fill, height fill ]
        { onChange = SystemforslagChanged icontext matchnummer
        , icon = checkboxIcon icontext forslag anvandarforslag
        , checked = anvandarforslag
        , label = Input.labelHidden labelText
        }


checkboxIcon : String -> Bool -> Bool -> Bool -> Element msg
checkboxIcon labeltext systemforslag anvandarforslag isChecked =
    el
        [ width <| px 30
        , height <| px 30
        , centerY
        , centerX
        , padding 4
        , if anvandarforslag then
            Border.color <| rgba255 150 0 0 0.7

          else
            Border.color <| rgb255 0xC0 0xC0 0xC0
        , Background.color <| rgba255 0 0 0 0
        , Border.rounded 6
        , Border.width 2
        ]
    <|
        el
            [ width fill
            , height fill
            , Border.rounded 4
            , Background.color <|
                if systemforslag then
                    rgb255 114 159 207

                else
                    rgb255 0xFF 0xFF 0xFF
            ]
        <|
            text labeltext


laddningsSkarm : Element Msg
laddningsSkarm =
    el [ alignBottom ] <|
        Element.html
            (Loading.render
                BouncingBalls
                { defaultConfig | color = "#333" }
                Loading.On
            )


view : Model -> Html Msg
view model =
    case model of
        Selection ->
            let
                borderedColumn =
                    column
                        [ width fill
                        , height fill
                        , scrollbarY
                        , padding 25
                        , spacing 5
                        , Font.size 16
                        , Border.width 2
                        , Border.rounded 6
                        ]
            in
            layout [] <|
                column [ height fill, width fill ]
                    [ header
                    , row [ width fill, height (px 500), padding 100, spacing 10 ]
                        [ borderedColumn
                            [ el [ centerX, Font.size 28, height <| px 50 ] <| text "Tipset"
                            , paragraph [] [ text "Stryktipset/Europatipset samt Topptipset." ]
                            , Input.button
                                [ Border.rounded 9
                                , Border.width 3
                                , centerX
                                , alignBottom
                                , padding 25
                                ]
                                { label = text "Mot jackpotten", onPress = Just SelectedTipset }
                            ]
                        , borderedColumn
                            [ el [ centerX, Font.size 28, height <| px 50 ] <| text "Över/under 1.5"
                            , paragraph [] [ text "Analys över dagens matcher för över/under." ]
                            , Input.button
                                [ Border.rounded 9
                                , Border.width 3
                                , centerX
                                , alignBottom
                                , padding 25
                                ]
                                { label = text "Ta mig till över/under", onPress = Just SelectedOverUnder }
                            ]
                        ]
                    ]

        LoadingTips _ ->
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
                        , laddningsSkarm
                        ]
                    ]

        LoadingOverunder ->
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
                            (text "Laddar in över/under")
                        , laddningsSkarm
                        ]
                    ]

        SuccessTips results ->
            layout [] <|
                column [ height fill, width fill ]
                    [ header
                    , mainTipsView results
                    ]

        SuccessOverUnder results ->
            layout [] <|
                column [ height fill, width fill ]
                    [ header
                    , mainOverUnderView results
                    ]

        Failure ->
            layout [] <| text "Det gick inte så bra att hitta en kupong."


matchFixtureView : Match -> Element Msg
matchFixtureView match =
    column [ width fill ]
        [ el [ Font.size 10 ] <|
            text match.liga
        , el
            []
          <|
            text match.hemmalag
        , el
            []
          <|
            text match.bortalag
        ]


overunderGoalPercentageView : String -> Element Msg
overunderGoalPercentageView probability =
    el [ height fill, width fill, centerX, Font.center ] <| text (String.slice 0 5 probability ++ "%")


recentGameBox : Match -> Element Msg
recentGameBox match =
    case match.resultat of
        Just r ->
            let
                backgroundColor =
                    case r.hemmalag + r.bortalag of
                        0 ->
                            rgb255 255 78 78

                        1 ->
                            rgb255 255 200 200

                        2 ->
                            rgb255 192 226 206

                        _ ->
                            rgb255 80 255 150
            in
            column []
                [ el
                    [ width (px 20)
                    , height (px 20)
                    , Border.rounded 5
                    , Background.color <| backgroundColor
                    , Font.center
                    , Font.size 16
                    ]
                  <|
                    Input.button [ centerX, centerY, Font.center ]
                        { label =
                            text
                                (String.fromInt
                                    (r.hemmalag
                                        + r.bortalag
                                    )
                                )
                        , onPress = Nothing
                        }
                ]

        _ ->
            Element.none


toResultatString : Maybe Score -> String
toResultatString score =
    (score
        |> Maybe.map (\s -> s.hemmalag)
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "NA"
    )
        ++ " - "
        ++ (score
                |> Maybe.map (\s -> s.bortalag)
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "NA"
           )


moreinfoRecentGameView : Match -> Element Msg
moreinfoRecentGameView resultat =
    row
        [ width fill, spacing 10, Font.size 12 ]
        [ el [] <| text (String.slice 2 10 resultat.date)
        , el [ width (px 110) ] <| text resultat.hemmalag
        , text " - "
        , el [ width (px 110) ] <| text resultat.bortalag
        , el [ Font.alignRight, alignRight ] <| text (toResultatString resultat.resultat)
        ]


modalColumn : List (Element msg) -> Element msg
modalColumn =
    column
        [ Background.color <| rgb255 255 255 255
        , Border.rounded 5
        , Border.width 2
        , padding 10
        , spacing 5
        ]


recentGamesView : OverUnderItem -> OverUnderModalState -> Element Msg
recentGamesView overUnderItem visaModal =
    let
        modalElement =
            case visaModal of
                NoneModal ->
                    Element.none

                HemmalagModal ma ->
                    if ma == overUnderItem.match then
                        modalColumn <| List.map moreinfoRecentGameView overUnderItem.senasteHemma

                    else
                        Element.none

                BortalagModal ma ->
                    if ma == overUnderItem.match then
                        modalColumn <| List.map moreinfoRecentGameView overUnderItem.senasteBorta

                    else
                        Element.none
    in
    column [ Element.above modalElement, Element.centerX ]
        [ row
            [ spacing 2
            , onMouseEnter <| OverUnderVisaModal (HemmalagModal overUnderItem.match)
            , onMouseLeave <| OverUnderVisaModal NoneModal
            ]
          <|
            List.map recentGameBox overUnderItem.senasteHemma
        , row
            [ spacing 2
            , onMouseEnter <| OverUnderVisaModal (BortalagModal overUnderItem.match)
            , onMouseLeave <| OverUnderVisaModal NoneModal
            ]
          <|
            List.map recentGameBox overUnderItem.senasteBorta
        ]


overUnderItemView : OverUnderItem -> OverUnderModalState -> Element Msg
overUnderItemView ouAnalys modalstate =
    row [ width fill ]
        [ row [ width fill ]
            [ matchFixtureView ouAnalys.match
            , recentGamesView ouAnalys modalstate
            ]

        -- Det här är såklart slappt. Men orkar inte kolla upp korrekt avrundning
        , row [ width fill, height fill, centerY, centerX ] <| List.map overunderGoalPercentageView ouAnalys.poisson
        ]


overUnderHeaderView : Element Msg
overUnderHeaderView =
    row [ width fill, height <| px 100, Border.widthEach { bottom = 3, top = 0, left = 0, right = 0 } ]
        [ row [ width fill, height fill ]
            [ el [ width fill, Font.size 28 ] <| text "Analys"
            , el [ width fill, Font.alignRight, padding 20 ] <| text "Senaste fem"
            ]
        , column [ height fill, width fill ]
            [ row [] [ el [ width fill, Font.center, spacing 10, alignTop ] <| text "Över/under mål" ]
            , row
                [ height fill, width fill, Font.size 20, Font.bold, Font.center ]
                [ el [ width fill ] <| text "0.5"
                , el [ width fill ] <| text "1.5"
                , el [ width fill ] <| text "2.5"
                , el [ width fill ] <| text "3.5"
                ]
            ]
        ]


mainOverUnderView : OverUnder -> Element Msg
mainOverUnderView overunder =
    let
        borderedColumn =
            column
                [ width fill
                , height fill
                , scrollbarY
                , padding 5
                , spacing 5
                , Font.size 16
                , Border.width 2
                , Border.rounded 6
                , Border.color <| rgba255 150 0 0 1
                ]
    in
    row [ width fill, height fill, padding 50, spacing 10 ]
        [ borderedColumn
            [ overUnderHeaderView
            , column [ width fill, paddingXY 0 5, spacing 10 ] <| List.map (\ou -> overUnderItemView ou overunder.overUnderModal) overunder.data
            ]
        ]


analyzeView : KupongRad -> Element Msg
analyzeView rad =
    case rad.analys of
        Just analys ->
            case analys.poisson of
                Just poisson ->
                    column [ width fill, height fill, padding 10, Border.rounded 10, Background.color <| rgb255 51 255 128 ]
                        [ column [ centerX, Font.bold ]
                            [ el [ centerX, Font.extraLight ] <| text rad.liga
                            , el [ centerX ] <| text (rad.home ++ " - " ++ rad.away)
                            ]
                        , column []
                            [ el [] <| text ("Predicted score: " ++ String.fromInt poisson.predictedScore.hemmalag ++ " - " ++ String.fromInt poisson.predictedScore.bortalag)
                            , el [] <| text ("Poissonanalys win/draw/win: " ++ poisson.outcomePercentage.hemmalag ++ " " ++ poisson.outcomePercentage.kryss ++ " " ++ poisson.outcomePercentage.bortalag)
                            , case rad.odds of
                                Just o ->
                                    el [] <| text ("Odds" ++ " 1: " ++ o.hemmalag ++ " X: " ++ o.kryss ++ " 2: " ++ o.bortalag)

                                Nothing ->
                                    Element.none
                            , el [] <| text ("Svenska folket: " ++ rad.svenskaFolket.hemmalag ++ "%" ++ " " ++ rad.svenskaFolket.kryss ++ "%" ++ " " ++ rad.svenskaFolket.bortalag ++ "%")
                            ]
                        , column [ width fill, height (px 300), Font.center ] <|
                            headerRow 5
                                :: List.take 6 (List.indexedMap poissonTableRowView poisson.poissonTable)
                        ]

                Nothing ->
                    text "Kunde inte analysera den här matchen. Försök igen senare."

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


boolTillTecken : Bool -> String -> String
boolTillTecken matchteckenBool tecken =
    if matchteckenBool then
        tecken

    else
        ""


radforslagTillStrang : MatchInfoHallareBool -> String
radforslagTillStrang forslag =
    boolTillTecken forslag.hemmalag "1"
        ++ boolTillTecken forslag.kryss "x"
        ++ boolTillTecken forslag.bortalag "2"



--- Tyvärr så måste jag köra en magisk "+1 på matcherna här eftersom det känns lite orimligt att skicka det med start på match 0..."


encodaForslagslista : Radforslag -> List (List ( String, Encode.Value ))
encodaForslagslista forslag =
    List.map
        (\data ->
            [ ( "nr", Encode.int (Tuple.first data + 1) )
            , ( "forslag", Encode.string (radforslagTillStrang (Tuple.second data)) )
            ]
        )
        (Dict.toList forslag)


forslagEncoder : Radforslag -> Encode.Value
forslagEncoder forslag =
    Encode.object
        [ ( "rad"
          , Encode.list Encode.object
                (encodaForslagslista forslag)
          )
        ]


scoreDecoder : D.Decoder Score
scoreDecoder =
    D.map2 Score
        (D.field "hemmalag" D.int)
        (D.field "bortalag" D.int)


resultatScoreDecoder : D.Decoder Score
resultatScoreDecoder =
    D.map2 Score
        (D.index 0 D.int)
        (D.index 1 D.int)


poissonDecoder : D.Decoder Poissonanalys
poissonDecoder =
    D.map3
        Poissonanalys
        (D.field "predictedScore" scoreDecoder)
        (D.field "outcomePercentage" matchInfoHallareDecoder)
        (D.field "poissonTable" (D.list (D.list D.float)))


analysDecoder : D.Decoder Analys
analysDecoder =
    D.map2
        Analys
        (optionalField "poisson" poissonDecoder)
        (optionalField "radforslag" matchInfoHallareBoolDecoder)


matchInfoHallareDecoder : D.Decoder MatchInfoHallare
matchInfoHallareDecoder =
    D.map3
        MatchInfoHallare
        (D.field "hemmalag" D.string)
        (D.field "kryss" D.string)
        (D.field "bortalag" D.string)


matchInfoHallareBoolDecoder : D.Decoder MatchInfoHallareBool
matchInfoHallareBoolDecoder =
    D.map3
        MatchInfoHallareBool
        (D.field "hemmalag" D.bool)
        (D.field "kryss" D.bool)
        (D.field "bortalag" D.bool)


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


matchDecoder : D.Decoder Match
matchDecoder =
    D.map5
        Match
        (D.field "homeTeam" D.string)
        (D.field "awayTeam" D.string)
        (D.field "liga" D.string)
        (D.field "fullTime" (D.nullable resultatScoreDecoder))
        (D.field "date" D.string)


overUnderItemDecoder : D.Decoder OverUnderItem
overUnderItemDecoder =
    D.map4
        OverUnderItem
        (D.field "match" matchDecoder)
        (D.field "poisson" (D.list (D.float |> D.map String.fromFloat)))
        (D.field "senasteHemma" (D.list matchDecoder))
        (D.field "senasteBorta" (D.list matchDecoder))


overUnderDecoder : D.Decoder OverUnder
overUnderDecoder =
    D.map2
        OverUnder
        (D.field "data"
            (D.list overUnderItemDecoder)
        )
        (D.succeed NoneModal)


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
