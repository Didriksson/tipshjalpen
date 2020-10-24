module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Debug exposing (log)
import Html.Events exposing (onClick)
import Html exposing (button)
import Http
import Json.Decode as D
import Html exposing (span)
import Html exposing (li)
import Html exposing (ul)


-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- MODEL


type alias Model = 
  {
    request : AnalyzeRequest,
    result  : Maybe (List AnalyzeResult)
  }

type alias AnalyzeRequest =
    { home : String
    , away : String
    }

type alias AnalyzeResult =
    { 
      predictedScore : String
      ,hometeam : String
      ,awayteam : String
    }

init : () -> (Model, Cmd Msg)
init _=
  ((Model (AnalyzeRequest "" "") Nothing), Cmd.none)



-- UPDATE
type Msg
  =   Changehome String
    | Changeaway String
    | SendAnalyzeRequest
    | GotAnalyzeResult (Result Http.Error (List AnalyzeResult))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Changehome newContent ->
      ({model | request = (AnalyzeRequest newContent model.request.away)}, Cmd.none)
    Changeaway newContent ->
      ({model | request = (AnalyzeRequest model.request.home newContent )}, Cmd.none)
    SendAnalyzeRequest ->
      (model, Http.get
      { url = "./data.json"
      , expect = Http.expectJson GotAnalyzeResult analyzeListDecoder
      })
    GotAnalyzeResult request ->
      case request of
        Ok re ->
          ({model | result = Just re}, Cmd.none)
        Err err ->
          (model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Hometeam", value model.request.home, onInput Changehome ] []
    , input [ placeholder "Awayteam", value model.request.away, onInput Changeaway ] []
    , button [onClick SendAnalyzeRequest] [ text "Send it!" ]
    , case model.result of
        Maybe.Just res ->
          ul []
            (List.map (\l -> analyzeView l) res)
        Nothing ->
          span [][]

    ]

analyzeView : AnalyzeResult -> Html Msg
analyzeView res =
  li [] [
    div [] [
      text res.predictedScore]
    , text res.hometeam
    , text "-"
    , text res.awayteam]


analyzeDecoder : D.Decoder AnalyzeResult
analyzeDecoder = 
  D.map3 AnalyzeResult
    (D.field "predictedScore" D.string)
    (D.field "hometeam" D.string)    
    (D.field "awayteam" D.string)

analyzeListDecoder : D.Decoder (List AnalyzeResult)
analyzeListDecoder =
  D.list analyzeDecoder