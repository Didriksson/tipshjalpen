module Main exposing (..)

import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, header, img, input, li, p, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (request)
import Json.Decode as D
import Maybe exposing (Maybe)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Model
    = InputMatches (Dict Int AnalyzeRequest)
    | Loading
    | Success (List AnalyzeResult)
    | Failure


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
    ( InputMatches
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
    , Cmd.none
    )



-- UPDATE


type Msg
    = Changehome Int String
    | Changeaway Int String
    | AddMatch
    | SendAnalyzeRequest
    | GotAnalyzeResult (Result Http.Error (List AnalyzeResult))



{- 
| GotAnalyzeResult (Result Http.Error (List AnalyzeResult))
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of ->
        GotAnalyzeResult result
    case model of
        Success items ->
            (Success , Cmd.none)
        Failure ->
            (Failure, Cmd.none)
        Loading ->
            case msg of
                GotAnalyzeResult result ->
                    case result of
                        Ok received ->
                            (Success received, Cmd.none)
                        Err _ ->
                            (Failure, Cmd.none)
        InputMatches matches ->
            case msg of
                Changehome index newContent ->
                    let
                        updateChange =
                            Maybe.map (\data -> { data | home = newContent })

                        changesUpdated =
                            Dict.update index
                                updateChange
                                matches
                    in
                    ( InputMatches changesUpdated, Cmd.none )

                Changeaway index newContent ->
                    let
                        updateChange =
                            Maybe.map (\data -> { data | away = newContent })

                        changesUpdated =
                            Dict.update index
                                updateChange
                                matches
                    in
                    ( InputMatches changesUpdated, Cmd.none )

                AddMatch ->
                    let
                        itemsAdded =
                            Dict.insert (Dict.size matches) (AnalyzeRequest "" "") matches
                    in
                    ( InputMatches itemsAdded, Cmd.none )
                SendAnalyzeRequest ->
                    ( Loading
                    , Http.get
                        { url = "./data.json"
                        , expect = Http.expectJson GotAnalyzeResult analyzeListDecoder
                        }
                    )
            


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        InputMatches matches ->
            div []
                [ header [] [ img [ src "./assets/logo.png" ] [] ]
                , div []
                    [ div [] (List.map inputView (Dict.toList matches))
                    , button [ onClick AddMatch ] [ text "+" ]

                    {- , button [ onClick SendAnalyzeRequest ] [ text "Send it!" ] -}
                    ]
                ]


inputView : ( Int, AnalyzeRequest ) -> Html Msg
inputView ( key, val ) =
    li []
        [ input [ placeholder "Hometeam", value val.home, onInput (Changehome key) ] []
        , input [ placeholder "Awayteam", value val.away, onInput (Changeaway key) ] []
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