import Html exposing
  ( Html, text
  , h1, h2, div, textarea, button, p, a
  , table, tbody, thead, tr, th, td
  , input, select, option, header, nav
  , span, section, nav, img, label
  )
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import Http
import Tuple exposing (..)
import Platform.Sub as Sub
import Array exposing (Array)
import Maybe exposing (withDefault)
import Json.Decode as J

import Ports exposing (..)
import Thing exposing (..)
import Helpers exposing (..)


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
type alias Model =
  { pos : Int
  , things : Array (Thing, Bool)
  , testnet : Bool
  , last_ops : List Op
  , last_txns : List Txn
  , last_leds : List Led
  }

type alias Flags =
  { testnet : Bool
  }


init : Flags -> (Model, Cmd Msg)
init flags =
  ( Model
    1
    (Array.fromList [ emptyThing, emptyThing ])
    flags.testnet
    []
    []
    []
  , sse <| base flags.testnet
  )


-- UPDATE
type Msg
  = GotThing Bool Int (Result Http.Error Thing)
  | Navigate Int String
  | Surf Int
  | Refresh Int
  | Pasted String
  | ToggleTestnet Bool
  | NewOperation Op
  | NewTransaction Txn
  | NewLedger Led
  | DoNothing 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotThing testnet pos result ->
      case result of
        Err err ->
          ( { model | things = model.things
              |> Array.set pos (Errored err, testnet)
            }
          , Cmd.none
          )
        Ok thing ->
          ( { model | things = model.things
              |> Array.set pos (thing, testnet)
            }
          , Cmd.none
          )
    Navigate base_pos pathname ->
      ( { model
          | pos = base_pos + 1
          , things = model.things
            |> Array.slice 0 (base_pos + 1)
            |> Array.push loadingThing
        }
      , Cmd.batch
        [ fetch (base model.testnet) pathname
          <| GotThing model.testnet (base_pos + 1)
        , pushPage (model.testnet, (base_pos + 1), pathname, pathname)
        ]
      )
    Pasted something ->
      let
        kind = identifierKind something
        pathname = "/" ++ kind ++ "/" ++ something
      in
        update (Navigate model.pos pathname) model
    Surf pos ->
      ( { model | pos = Debug.log "surfing to" ( if pos <= 1 then 1 else pos ) }
      , Cmd.none
      )
    Refresh pos ->
      let
        pathname = model.things
          |> Array.get pos
          |> withDefault emptyThing
          |> first
          |> thingUrl
      in
        ( model
        , fetch (base model.testnet) pathname
          <| GotThing model.testnet (pos + 1)
        )
    ToggleTestnet testnet ->
      ( { model | testnet = Debug.log "testnet?" testnet }
      , Cmd.none
      )
    NewOperation op ->
      ( { model | last_ops = List.take 23 (op :: model.last_ops) }
      , Cmd.none
      )
    NewTransaction txn ->
      ( { model | last_txns = List.take 23 (txn :: model.last_txns) }
      , Cmd.none
      )
    NewLedger led ->
      ( { model | last_leds = List.take 23 (led :: model.last_leds) }
      , Cmd.none
      )
    DoNothing ->
      ( model, Cmd.none )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ navigate (Navigate model.pos)
    , surf Surf
    , newop <| J.decodeString opDecoder >> Result.withDefault defaultOp >> NewOperation
    , newtxn <| J.decodeString txnDecoder >> Result.withDefault defaultTxn >> NewTransaction
    , newled <| J.decodeString ledDecoder >> Result.withDefault defaultLed >> NewLedger
    ]


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ div [ class "top columns is-mobile" ]
      [ div [ class "column is-12" ]
        [ input
          [ class "input"
          , placeholder "Paste a Stellar identifier (address, transaction hash, ledger number, operation id etc.)"
          , onInput Pasted
          ] []
        ]
      ]
    , div [ class "testnet-switch" ]
      [ input
        [ type_ "checkbox"
        , id "toggle-testnet"
        , checked <| not model.testnet
        , onCheck ToggleTestnet
        ] []
      , label [ for "toggle-testnet" ] [ text <| if model.testnet then "test" else "public" ]
      ]
    , div [ class "main columns" ] <|
      let 
        before = Array.get (model.pos - 2) model.things |> withDefault emptyThing
        left = Array.get (model.pos - 1) model.things |> withDefault emptyThing
        right = Array.get (model.pos) model.things |> withDefault emptyThing
        after = Array.get (model.pos + 1) model.things |> withDefault emptyThing
      in
        [ div [ class "column is-2 is-hidden-touch" ]
          [ viewThing (Surf <| model.pos - 2) (Navigate (model.pos - 2)) before
          ]
        , div [ class "column is-hidden-mobile" ]
          [ viewThing (Surf <| model.pos - 1) (Navigate (model.pos - 1)) left
          ]
        , div [ class "column" ]
          [ viewThing DoNothing (Navigate model.pos) right
          ]
        , div [ class "column is-2 is-hidden-touch" ]
          [ viewThing (Surf <| model.pos + 1) (Navigate (model.pos + 1)) after
          ]
        ]
    , div [ class "live columns" ]
      [ div [ class "column" ]
        [ div [ class "box" ]
          [ h1 [ class "title is-4" ] [ text "last operations" ]
          , table []
            [ thead []
              [ tr []
                [ th [] [ text "id" ]
                , th [] [ text "type" ]
                , th [] [ text "source" ]
                ]
              ]
            , tbody []
              <| List.map (shortOpRow <| Navigate model.pos) model.last_ops
            ]
          ]
        ]
      , div [ class "column" ]
        [ div [ class "box" ]
          [ h1 [ class "title is-4" ] [ text "last transactions" ]
          , table []
            [ thead []
              [ tr []
                [ th [] [ text "hash" ]
                , th [] [ text "time" ]
                , th [] [ text "source" ]
                , th [] [ text "ops" ]
                ]
              ]
            , tbody []
              <| List.map (shortTxnRow <| Navigate model.pos) model.last_txns
            ]
          ]
        ]
      , div [ class "column" ]
        [ div [ class "box" ]
          [ h1 [ class "title is-4" ] [ text "last ledgers closed" ]
          , table []
            [ thead []
              [ tr []
                [ th [] [ text "seq" ]
                , th [] [ text "time" ]
                , th [] [ text "txns" ]
                , th [] [ text "ops" ]
                ]
              ]
            , tbody []
              <| List.map (shortLedRow <| Navigate model.pos) model.last_leds
            ]
          ]
        ]
      ]
    ]
