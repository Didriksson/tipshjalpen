module Main exposing (..)

-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Debug exposing (log)
import Html.Events exposing (onClick)
import Html exposing (button)
import Http
import Json.Decode as D


-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- MODEL


type  Model =
    Request AnalyzeRequest
  | Result  AnalyzeResult

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
  (Request (AnalyzeRequest "" ""), Cmd.none)



-- UPDATE
type Msg
  =   Changehome String
    | Changeaway String
    | SendAnalyzeRequest
    | GotAnalyzeResult (Result Http.Error AnalyzeResult)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Changehome newContent ->
      ({ model | hometeam = newContent }, Cmd.none)
    Changeaway newContent ->
      ({ model | awayteam = newContent }, Cmd.none)
    SendAnalyzeRequest ->
      (model, Http.get
      { url = "./data.json"
      , expect = Http.expectJson GotAnalyzeResult analyzeDecoder
      })
    GotAnalyzeResult request ->
      case request of
        Ok result ->
          ({model | analyze = result}, Cmd.none)
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
    [ input [ placeholder "Hometeam", value model.hometeam, onInput Changehome ] []
    , input [ placeholder "Awayteam", value model.awayteam, onInput Changeaway ] []
    , button [onClick SendAnalyzeRequest] [ text "Send it!" ]
    , text model.analyze.predictedScore
    ]


analyzeDecoder : D.Decoder AnalyzeResult
analyzeDecoder = 
  D.map3 AnalyzeResult
    (D.field "predictedScore" D.string)
    (D.field "hometeam" D.string)    
    (D.field "awayteam" D.string)