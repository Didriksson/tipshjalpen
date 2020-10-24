module Main exposing (..)

import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, header, img, input, li, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (request)
import Json.Decode as D
import Maybe exposing (Maybe)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { request : Dict Int AnalyzeRequest
    , result : Maybe (List AnalyzeResult)
    }


type alias AnalyzeRequest =
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
    ( Model
        (Dict.fromList
            [ ( 0, AnalyzeRequest "" "" )
            , ( 1, AnalyzeRequest "" "" )
            , ( 2, AnalyzeRequest "" "" )
            , ( 3, AnalyzeRequest "" "" )
            , ( 4, AnalyzeRequest "" "" )
            , ( 5, AnalyzeRequest "" "" )
            , ( 6, AnalyzeRequest "" "" )
            , ( 7, AnalyzeRequest "" "" )
            , ( 8, AnalyzeRequest "" "" )
            , ( 9, AnalyzeRequest "" "" )
            , ( 10, AnalyzeRequest "" "" )
            , ( 11, AnalyzeRequest "" "" )
            , ( 12, AnalyzeRequest "" "" )
            , ( 13, AnalyzeRequest "" "" )
            ]
        )
        Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = Changehome Int String
    | Changeaway Int String
    | SendAnalyzeRequest
    | AddAnalyzeRequest
    | GotAnalyzeResult (Result Http.Error (List AnalyzeResult))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddAnalyzeRequest ->
            let
                itemsAdded =
                    Dict.insert (Dict.size model.request) (AnalyzeRequest "" "") model.request
            in
            ( { model | request = itemsAdded }, Cmd.none )

        Changehome index newContent ->
            let
                updateChange =
                    Maybe.map (\data -> { data | home = newContent })

                changesUpdated =
                    Dict.update index
                        updateChange
                        model.request
            in
            ( { model | request = changesUpdated }, Cmd.none )

        Changeaway index newContent ->
            let
                updateChange =
                    Maybe.map (\data -> { data | away = newContent })

                changesUpdated =
                    Dict.update index
                        updateChange
                        model.request
            in
            ( { model | request = changesUpdated }, Cmd.none )

        SendAnalyzeRequest ->
            ( model
            , Http.get
                { url = "./data.json"
                , expect = Http.expectJson GotAnalyzeResult analyzeListDecoder
                }
            )

        GotAnalyzeResult request ->
            case request of
                Ok re ->
                    ( { model | result = Just re }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header [] [ img [ src "./assets/logo.png" ] [] ]
        , div []
            [ div [] (List.map inputView (Dict.toList model.request))
            , button [ onClick AddAnalyzeRequest ] [ text "+" ]
            , button [ onClick SendAnalyzeRequest ] [ text "Send it!" ]
            , analyzeView model
            ]
        ]


inputView : ( Int, AnalyzeRequest ) -> Html Msg
inputView ( key, val ) =
    li []
        [ input [ placeholder "Hometeam", value val.home, onInput (Changehome key) ] []
        , input [ placeholder "Awayteam", value val.away, onInput (Changeaway key) ] []
        ]


analyzeView : Model -> Html Msg
analyzeView model =
    case model.result of
        Maybe.Just res ->
            ul []
                (List.map (\l -> analyzeItemView l) res)

        Nothing ->
            span [] []


analyzeItemView : AnalyzeResult -> Html Msg
analyzeItemView res =
    li []
        [ div []
            [ text res.predictedScore
            ]
        , text res.hometeam
        , text "-"
        , text res.awayteam
        ]


analyzeDecoder : D.Decoder AnalyzeResult
analyzeDecoder =
    D.map3 AnalyzeResult
        (D.field "predictedScore" D.string)
        (D.field "hometeam" D.string)
        (D.field "awayteam" D.string)


analyzeListDecoder : D.Decoder (List AnalyzeResult)
analyzeListDecoder =
    D.list analyzeDecoder
