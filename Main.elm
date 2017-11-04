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
import Helpers exposing (..)


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
  }


init : (Model, Cmd Msg)
init =
  ( Model 1 (Array.fromList [ Empty, Empty ]) ""
  , Cmd.none
  )


-- UPDATE
type Msg
  = GotThing Int (Result Http.Error Thing)
  | Navigate Int String
  | Surf Int
  | Refresh Int
  | Pasted String
  | DoNothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotThing pos result ->
      case result of
        Err err ->
          ( { model | things = model.things |> Array.set pos (Errored err) }
          , Cmd.none
          )
        Ok thing ->
          ( { model | things = model.things |> Array.set pos (Debug.log "thing" thing) }
          , Cmd.none
          )
    Navigate base_pos pathname ->
      ( { model
          | pos = base_pos + 1
          , things = model.things
            |> Array.slice 0 (base_pos + 1)
            |> Array.push Loading
        }
      , Cmd.batch
        [ fetch pathname <| GotThing (base_pos + 1)
        , pushPage ((base_pos + 1), pathname, pathname)
        ]
      )
    Pasted something ->
      let
        kind = identifierKind something
        pathname = "/" ++ kind ++ "/" ++ something
      in
        update (Navigate model.pos pathname) model
    Surf pos ->
      ( { model | pos = Debug.log "surfing to" pos }
      , Cmd.none
      )
    Refresh pos ->
      let
        pathname = model.things
          |> Array.get pos
          |> withDefault Empty
          |> thingUrl
      in
        ( model
        , fetch pathname <| GotThing (pos + 1)
        )
    DoNothing ->
      ( model, Cmd.none )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ navigate (Navigate model.pos)
    , surf Surf
    ]


-- VIEW
view : Model -> Html Msg
view model =
  if model.error /= "" then text model.error
  else div []
    [ div [ class "top columns is-mobile" ]
      [ div [ class "column is-12" ]
        [ input
          [ class "input"
          , placeholder "Paste a Stellar identifier (address, transaction hash, ledger number, operation id etc.)"
          , onInput Pasted
          ] []
        ]
      ]
    , div [ class "main columns" ] <|
      let 
        before = Array.get (model.pos - 2) model.things |> withDefault Empty
        left = Array.get (model.pos - 1) model.things |> withDefault Empty
        right = Array.get (model.pos) model.things |> withDefault Empty
        after = Array.get (model.pos + 1) model.things |> withDefault Empty
      in
        [ div [ class "column is-2 is-hidden-touch" ]
          [ viewThing (Surf <| model.pos - 2) (Navigate (model.pos - 2)) before
          ]
        , div [ class "column is-hidden-mobile" ]
          [ viewThing DoNothing (Navigate (model.pos - 1)) left
          ]
        , div [ class "column" ]
          [ viewThing DoNothing (Navigate model.pos) right
          ]
        , div [ class "column is-2 is-hidden-touch" ]
          [ viewThing (Surf <| model.pos + 1) (Navigate (model.pos + 1)) after
          ]
        ]
    ]
