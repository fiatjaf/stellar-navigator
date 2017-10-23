import Html exposing
  ( Html, text
  , h1, h2, div, textarea, button, p, a
  , table, tbody, thead, tr, th, td
  , input, select, option, header, nav
  , span, section, nav, img, label
  )
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Http
import Platform.Sub as Sub
import Array exposing (Array)
import Maybe exposing (withDefault)

import Ports exposing (..)
import Thing exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
type alias Model =
  { pos : Int
  , things : Array Thing
  , error : String
  , loading : String
  }


init : (Model, Cmd Msg)
init =
  ( Model 1 (Array.fromList [ Empty, Empty ]) "" ""
  , Cmd.none
  )


-- UPDATE
type Msg
  = GotThing Int (Result Http.Error Thing)
  | Navigate String
  | Surf Int
  | Pasted String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotThing pos result ->
      case result of
        Err err -> ( { model | error = "ERROR" }, Cmd.none )
        Ok thing ->
          ( { model | things = model.things |> Array.set model.pos thing }
          , Cmd.none
          )
    Navigate pathname ->
      ( { model
          | pos = model.pos + 1
          , things = model.things
            |> Array.slice 0 (model.pos + 1)
            |> Array.push Empty
        }
      , fetch pathname <| GotThing (model.pos + 1)
      )
    Surf pos ->
      ( model, Cmd.none )
    Pasted something ->
      ( model, Cmd.none )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ navigate Navigate
    , surf Surf
    ]


-- VIEW
view : Model -> Html Msg
view model =
  if model.error /= "" then text model.error
  else div []
    [ div [ class "top" ]
      [ input
        [ class "input"
        , placeholder "paste some Stellar identifier here"
        , onInput Pasted
        ] []
      , button [ class "button is-primary" ] [ text "ok" ]
      ]
    , div [ class "main columns" ] <|
      let 
        left = Array.get (model.pos - 1) model.things |> withDefault Empty
        right = Array.get (model.pos) model.things |> withDefault Empty
      in
        [ div [ class "column" ] [ viewThing left ]
        , div [ class "column" ] [ viewThing right ]
        ]
    ]
