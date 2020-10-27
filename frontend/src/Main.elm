module Main exposing (..)

import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font exposing (alignLeft)
import Element.Input as Input
import Html exposing (Attribute, Html, button, div, header, img, input, li, p, span, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (request)
import Json.Decode as D
import Maybe exposing (Maybe)
import Regex



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Model
    = InputRequestState AnalyzeInput
    | Loading
    | Success (List AnalyzeResult)
    | Failure


type alias AnalyzeInput =
    { matches : Dict Int MatchInputBox
    , helgarderingar : String
    , halvgarderingar : String
    }


type alias MatchInputBox =
    { home : String
    , away : String
    }


type alias AnalyzeResult =
    { predictedScore : String
    , hometeam : String
    , awayteam : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( InputRequestState
        (AnalyzeInput
            (Dict.fromList
                [ ( 0, MatchInputBox "" "" )
                , ( 1, MatchInputBox "" "" )
                , ( 2, MatchInputBox "" "" )
                , ( 3, MatchInputBox "" "" )
                , ( 4, MatchInputBox "" "" )
                , ( 5, MatchInputBox "" "" )
                , ( 6, MatchInputBox "" "" )
                , ( 7, MatchInputBox "" "" )
                , ( 8, MatchInputBox "" "" )
                , ( 9, MatchInputBox "" "" )
                , ( 10, MatchInputBox "" "" )
                , ( 11, MatchInputBox "" "" )
                , ( 12, MatchInputBox "" "" )
                ]
            )
            "0"
            "0"
        )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Changehome Int String
    | Changeaway Int String
    | Changehelgarderingar String
    | Changehalvgarderingar String
    | AddMatch
    | SendAnalyzeRequest
    | GotAnalyzeResult (Result Http.Error (List AnalyzeResult))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Changehome index newContent ->
            case model of
                InputRequestState inputstate ->
                    let
                        updateChange =
                            Maybe.map (\data -> { data | home = newContent })

                        changesUpdated =
                            Dict.update index
                                updateChange
                                inputstate.matches
                    in
                    ( InputRequestState { inputstate | matches = changesUpdated }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Changeaway index newContent ->
            case model of
                InputRequestState inputstate ->
                    let
                        updateChange =
                            Maybe.map (\data -> { data | away = newContent })

                        changesUpdated =
                            Dict.update index
                                updateChange
                                inputstate.matches
                    in
                    ( InputRequestState { inputstate | matches = changesUpdated }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Changehalvgarderingar newContent ->
            case model of
                InputRequestState inputState ->
                    let
                        regexNotDigit =
                            Maybe.withDefault Regex.never <|
                                Regex.fromString "[^0-9]"

                        containsNonDigit =
                            Regex.contains regexNotDigit newContent
                    in
                    if newContent == "" then
                        ( InputRequestState { inputState | halvgarderingar = newContent }, Cmd.none )

                    else if containsNonDigit then
                        ( model, Cmd.none )

                    else if ((String.toInt newContent |> Maybe.withDefault 0) + (String.toInt inputState.helgarderingar |> Maybe.withDefault 0)) > Dict.size inputState.matches then
                        ( model, Cmd.none )

                    else
                        ( InputRequestState { inputState | halvgarderingar = newContent }, Cmd.none )


                _ ->
                    ( model, Cmd.none )

        Changehelgarderingar newContent ->
            case model of
                InputRequestState inputState ->
                    let
                        regexNotDigit =
                            Maybe.withDefault Regex.never <|
                                Regex.fromString "[^0-9]"

                        containsNonDigit =
                            Regex.contains regexNotDigit newContent
                    in
                    if newContent == "" then
                        ( InputRequestState { inputState | helgarderingar = newContent }, Cmd.none )

                    else if containsNonDigit then
                        ( model, Cmd.none )

                    else if ((String.toInt newContent |> Maybe.withDefault 0) + (String.toInt inputState.halvgarderingar |> Maybe.withDefault 0)) > Dict.size inputState.matches then
                        ( model, Cmd.none )

                    else
                        ( InputRequestState { inputState | helgarderingar = newContent }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AddMatch ->
            case model of
                InputRequestState inputstate ->
                    let
                        itemsAdded =
                            Dict.insert (Dict.size inputstate.matches) (MatchInputBox "" "") inputstate.matches
                    in
                    ( InputRequestState { inputstate | matches = itemsAdded }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SendAnalyzeRequest ->
            case model of
                InputRequestState _ ->
                    ( Loading
                    , Http.get
                        { url = "./data.json"
                        , expect = Http.expectJson GotAnalyzeResult analyzeListDecoder
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        GotAnalyzeResult result ->
            case model of
                Loading ->
                    case result of
                        Ok received ->
                            ( Success received, Cmd.none )

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
        [ height <| fillPortion 1
        , width fill
        , Background.color <| rgb255 92 99 118
        , Font.color <| rgb255 255 255 255
        ]
    <|
        text "Tipshjälpen"


inputMatchesView : AnalyzeInput -> Element Msg
inputMatchesView matches =
    column
        [ height <| fillPortion 5
        , width fill
        , centerY
        , Background.color <| rgb255 200 99 118
        , paddingEach { top = 20, left = 5, right = 5, bottom = 20 }
        , spacing 10
        ]
    <|
        List.map inputView (Dict.toList matches.matches)



{- div
   []
   [ header [] [ img [ src "./assets/logo.png" ] [] ]
   , div []
       [ div [] (List.map inputView (Dict.toList matches))
       , button [ onClick AddMatch ] [ text "+" ]
       , button [ onClick SendAnalyzeRequest ] [ text "Send it!" ]
       ]
   ]
-}


inputView : ( Int, MatchInputBox ) -> Element Msg
inputView ( key, val ) =
    row
        [ paddingEach { top = 5, left = 5, right = 20, bottom = 0 }
        , spacing 20
        , centerY
        , height shrink
        ]
        [ el [ padding 5, width (px 20) ] <| Element.text (String.fromInt (key + 1))
        , Input.text
            [ centerX
            , centerY
            ]
            { text = val.home
            , onChange = Changehome key
            , placeholder =
                Just
                    (Input.placeholder []
                        (text "Home")
                    )
            , label = Input.labelHidden "Hometeam input"
            }
        , Input.text
            [ centerX
            , centerY
            ]
            { text = val.away
            , onChange = Changeaway key
            , placeholder =
                Just
                    (Input.placeholder []
                        (text "Away")
                    )
            , label = Input.labelHidden "Awayteam input"
            }
        ]


mainView : AnalyzeInput -> Element Msg
mainView input =
    row [spacing 20, height fill, padding 10]
        [ inputMatchesView input
        , Input.text
            [width shrink]
            { text = input.helgarderingar
            , onChange = Changehelgarderingar
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "Helgarderingar")
            }
        , Input.text
            [width shrink]
            { text = input.halvgarderingar
            , onChange = Changehalvgarderingar
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "Halvgarderingar")
            }
        ]


view : Model -> Html Msg
view model =
    case model of
        InputRequestState matches ->
            layout [] <|
                column [ height fill, width fill ]
                    [ header
                    , mainView matches
                    ]

        Loading ->
            layout [] <| text "Loading...!"

        Success results ->
            ul []
                (List.map (\l -> analyzeView l) results)

        Failure ->
            layout [] <| text "Something is wrong...!"


analyzeView : AnalyzeResult -> Html Msg
analyzeView res =
    div [] []


analyzeDecoder : D.Decoder AnalyzeResult
analyzeDecoder =
    D.map3 AnalyzeResult
        (D.field "predictedScore" D.string)
        (D.field "hometeam" D.string)
        (D.field "awayteam" D.string)


analyzeListDecoder : D.Decoder (List AnalyzeResult)
analyzeListDecoder =
    D.list analyzeDecoder
