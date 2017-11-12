module Thing exposing (..)

import Html exposing
  ( Html, Attribute, text
  , h1, h2, div, textarea, button, p, a
  , table, tbody, thead, tr, th, td
  , input, select, option, header
  , span, section, img, label
  )
import Html.Attributes exposing (class, title, attribute, colspan)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Http
import Task
import Dict exposing (Dict)
import Json.Decode as J
import Route exposing (..)

import Operations exposing (..)
import Effects exposing (..)
import Asset exposing (..)
import Helpers exposing (..)


-- UTILS


routes = router
  [ route (Address << \s -> { defaultAddr | id = s })
    <| static "addr" </> string
  , route (TransactionsForAddress << \s -> { defaultTfA | addr = s })
    <| static "txnsforaddr" </> string
  , route (OperationsForAddress << \s -> { defaultOfA | addr = s })
    <| static "opsforaddr" </> string
  , route (EffectsForAddress << \s -> { defaultEfA | addr = s })
    <| static "effsforaddr" </> string
  , route (Transaction << \s -> { defaultTxn | hash = s })
    <| static "txn" </> string
  , route (OperationsForTransaction << \s -> { defaultOfT | hash = s })
    <| static "opsfortxn" </> string
  , route (EffectsForTransaction << \s -> { defaultEfT | hash = s })
    <| static "effsfortxn" </> string
  , route (Operation << \s -> { defaultOp | id = s })
    <| static "op" </> string
  , route (EffectsForOperation << \s -> { defaultEfO | id = s })
    <| static "effsforop" </> string
  , route (Ledger << \s -> { defaultLed | sequence = s })
    <| static "led" </> int
  , route (OperationsForLedger << \s -> { defaultOfL | sequence = s })
    <| static "opsforled" </> int
  , route (TransactionsForLedger << \s -> { defaultTfL | sequence = s })
    <| static "txnsforled" </> int
  , route (EffectsForLedger << \s -> { defaultEfL | sequence = s })
    <| static "effsforled" </> int
  ] 

match : String -> Maybe Thing
match = Route.match routes

fetch : String -> String -> (Result Http.Error Thing -> msg) -> Cmd msg
fetch base pathname hmsg =
  case match pathname of
    Just thing ->
      Http.get (base ++ (thingUrl thing)) (thingDecoder thing)
        |> Http.send hmsg
    Nothing ->
      hmsg (Ok Empty) |> Task.succeed |> Task.perform identity

type Thing
  = Address Addr
  | TransactionsForAddress TfA
  | OperationsForAddress OfA
  | EffectsForAddress EfA
  | Transaction Txn
  | OperationsForTransaction OfT
  | EffectsForTransaction EfT
  | Operation Op
  | EffectsForOperation EfO
  | Ledger Led
  | OperationsForLedger OfL
  | TransactionsForLedger TfL
  | EffectsForLedger EfL
  | Empty
  | Loading
  | Errored Http.Error

emptyThing = ( Empty, False )
loadingThing = ( Loading, False )

thingUrl : Thing -> String
thingUrl thing =
  case thing of
    Address addr -> "/accounts/" ++ addr.id
    TransactionsForAddress tfa ->
      "/accounts/" ++ tfa.addr ++ "/transactions?order=desc"
    OperationsForAddress ofa ->
      "/accounts/" ++ ofa.addr ++ "/operations?order=desc"
    EffectsForAddress efa ->
      "/accounts/" ++ efa.addr ++ "/effects?order=desc"
    Transaction txn -> "/transactions/" ++ txn.hash
    OperationsForTransaction oft ->
      "/transactions/" ++ oft.hash ++ "/operations"
    EffectsForTransaction eft ->
      "/transactions/" ++ eft.hash ++ "/effects?order=desc"
    Operation op -> "/operations/" ++ op.id
    EffectsForOperation efo ->
      "/operations/" ++ efo.id ++ "/effects?order=desc"
    Ledger led -> "/ledgers/" ++ (toString led.sequence)
    OperationsForLedger ofl ->
      "/ledgers/" ++ (toString ofl.sequence) ++ "/operations"
    TransactionsForLedger tfl ->
      "/ledgers/" ++ (toString tfl.sequence) ++ "/transactions"
    EffectsForLedger efl ->
      "/ledgers/" ++ (toString efl.sequence) ++ "/effects?order=desc"
    Empty -> ""
    Loading -> ""
    Errored _ -> ""

thingDecoder : Thing -> J.Decoder Thing
thingDecoder thing =
  case thing of
    Address _ -> addrDecoder |> J.map Address
    TransactionsForAddress tfa ->
      tfaDecoder
        |> J.map (\f -> { f | addr = tfa.addr })
        |> J.map TransactionsForAddress
    OperationsForAddress ofa ->
      ofaDecoder
        |> J.map (\f -> { f | addr = ofa.addr })
        |> J.map OperationsForAddress
    EffectsForAddress efa ->
      efaDecoder
        |> J.map (\f -> { f | addr = efa.addr })
        |> J.map EffectsForAddress
    Transaction _ -> txnDecoder |> J.map Transaction
    OperationsForTransaction oft ->
      oftDecoder
        |> J.map (\f -> { f | hash = oft.hash })
        |> J.map OperationsForTransaction
    EffectsForTransaction eft ->
      eftDecoder
        |> J.map (\f -> { f | hash = eft.hash })
        |> J.map EffectsForTransaction
    Operation _ -> opDecoder |> J.map Operation
    EffectsForOperation efo ->
      efoDecoder
        |> J.map (\f -> { f | id = efo.id })
        |> J.map EffectsForOperation
    Ledger _ -> ledDecoder |> J.map Ledger
    OperationsForLedger ofl ->
      oflDecoder
        |> J.map (\f -> { f | sequence = ofl.sequence })
        |> J.map OperationsForLedger
    TransactionsForLedger tfl ->
      tflDecoder
        |> J.map (\f -> { f | sequence = tfl.sequence })
        |> J.map TransactionsForLedger
    EffectsForLedger efl ->
      eflDecoder
        |> J.map (\f -> { f | sequence = efl.sequence })
        |> J.map EffectsForLedger
    Errored err -> J.null (Errored err)
    Loading -> J.null Loading
    Empty -> J.null Empty


-- MODELS


type alias Addr =
  { id : String
  , balances : List Balance
  , home_domain : String
  , inflation_destination : String
  , signers : List Signer
  , data : Dict String String
  }

defaultAddr = Addr "" [] "" "" [] (Dict.fromList [])

addrDecoder : J.Decoder Addr
addrDecoder =
  J.map6 Addr
    ( J.field "id" J.string )
    ( J.field "balances" <| J.list balanceDecoder )
    ( J.map (Maybe.withDefault "")
      <| J.maybe ( J.field "home_domain" J.string )
    )
    ( J.map (Maybe.withDefault "")
      <| J.maybe ( J.field "inflation_destination" J.string )
    )
    ( J.field "signers"
      <| J.list
      <| J.map4 Signer
        ( J.field "public_key" J.string )
        ( J.field "weight" J.int )
        ( J.field "key" J.string )
        ( J.field "type" J.string )
    )
    ( J.field "data" (J.dict J.string) )

type alias Signer =
  { public_key : String
  , weight : Int
  , key : String
  , type_ : String
  }

type alias Balance =
  { asset : Asset
  , balance : String
  , limit : String
  }

balanceDecoder : J.Decoder Balance
balanceDecoder =
  J.map3 Balance
    ( assetDecoder )
    ( J.field "balance" J.string )
    ( J.oneOf
      [ J.field "limit" J.string
      , J.succeed ""
      ]
    )

type alias TfA = { addr : String , transactions : List Txn }
defaultTfA = TfA "" []
tfaDecoder =
  J.map2 TfA
    ( J.succeed "" )
    ( J.at ["_embedded", "records"] (J.list txnDecoder) )

type alias OfA = { addr : String , operations : List Op }
defaultOfA = OfA "" []
ofaDecoder =
  J.map2 OfA
    ( J.succeed "" )
    ( J.at ["_embedded", "records"] (J.list opDecoder) )

type alias EfA = { addr : String , effects : List Eff }
defaultEfA = EfA "" []
efaDecoder =
  J.map2 EfA
    ( J.succeed "" )
    ( J.at ["_embedded", "records"] (J.list effDecoder) )


type alias Txn =
  { hash : String
  , ledger : Int
  , created_at : String
  , source_account : String
  , operation_count : Int
  , fee_paid : Int
  }

defaultTxn = Txn "" 0 "" "" 0 0

txnDecoder : J.Decoder Txn
txnDecoder =
  J.map6 Txn
    ( J.field "hash" J.string )
    ( J.field "ledger" J.int )
    ( J.field "created_at" J.string )
    ( J.field "source_account" J.string )
    ( J.field "operation_count" J.int )
    ( J.field "fee_paid" J.int )

type alias OfT = { hash : String , operations : List Op }
defaultOfT = OfT "" []
oftDecoder =
  J.map2 OfT
    ( J.succeed "" )
    ( J.at ["_embedded", "records"] (J.list opDecoder) )

type alias EfT = { hash : String , effects : List Eff }
defaultEfT = EfT "" []
eftDecoder =
  J.map2 EfT
    ( J.succeed "" )
    ( J.at ["_embedded", "records"] (J.list effDecoder) )


type alias Op =
  { id : String
  , source_account : String
  , type_ : String
  , txn : String
  , data : OpData
  }

defaultOp = Op "" "" "" "" Noop

opDecoder : J.Decoder Op
opDecoder =
  J.map5 Op
    ( J.field "id" J.string )
    ( J.field "source_account" J.string )
    ( J.field "type" J.string )
    ( J.at [ "_links", "transaction", "href" ] J.string
      |> J.map (String.split "/" >> List.reverse >> List.head >> Maybe.withDefault "")
    )
    ( ( J.field "type" J.string )
      |> J.andThen opDataDecoder
    )

type alias EfO = { id : String , effects : List Eff }
defaultEfO = EfO "" []
efoDecoder =
  J.map2 EfO
    ( J.succeed "" )
    ( J.at ["_embedded", "records"] (J.list effDecoder) )


type alias Led =
  { sequence : Int
  , hash : String
  , transaction_count : Int
  , operation_count : Int
  , closed_at : String
  , network : LedNetwork
  }

type alias LedNetwork =
  { total_coins : String
  , fee_pool : String
  , base_fee : Int
  , base_reserve : String
  , max_tx_set_size : Int
  , protocol_version : Int
  }

defaultLed = Led 0 "" 0 0 "" (LedNetwork "" "" 0 "" 0 0)

networkDecoder : J.Decoder LedNetwork
networkDecoder =
  J.map6 LedNetwork
    ( J.field "total_coins" J.string )
    ( J.field "fee_pool" J.string )
    ( J.field "base_fee" J.int )
    ( J.field "base_reserve" J.string )
    ( J.field "max_tx_set_size" J.int )
    ( J.field "protocol_version" J.int )

ledDecoder : J.Decoder Led
ledDecoder =
  J.map6 Led
    ( J.field "sequence" J.int )
    ( J.field "hash" J.string )
    ( J.field "transaction_count" J.int )
    ( J.field "operation_count" J.int )
    ( J.field "closed_at" J.string )
    ( networkDecoder )

type alias TfL = { sequence : Int , transactions : List Txn }
defaultTfL = TfL 0 []
tflDecoder =
  J.map2 TfL
    ( J.succeed 0 )
    ( J.at ["_embedded", "records"] (J.list txnDecoder) )

type alias OfL = { sequence : Int , operations : List Op }
defaultOfL = OfL 0 []
oflDecoder =
  J.map2 OfL
    ( J.succeed 0 )
    ( J.at ["_embedded", "records"] (J.list opDecoder) )

type alias EfL = { sequence : Int , effects : List Eff }
defaultEfL = EfL 0 []
eflDecoder =
  J.map2 EfL
    ( J.succeed 0 )
    ( J.at ["_embedded", "records"] (J.list effDecoder) )


type alias Eff =
  { id : String
  , type_ : String
  , account : String
  , operation : String
  , effData : EffData
  }

defaultEff = Eff "" "" "" "" NoEffData

effDecoder : J.Decoder Eff
effDecoder =
  J.map5 Eff
    ( J.field "id" J.string )
    ( J.field "type" J.string )
    ( J.field "account" J.string )
    ( J.at [ "_links", "operation", "href" ] J.string
      |> J.map (String.split "/" >> List.reverse >> List.head >> Maybe.withDefault "")
    )
    ( ( J.field "type" J.string )
      |> J.andThen effDataDecoder
    )


-- VIEWS


viewThing : (Thing, Bool) -> Html GlobalAction
viewThing (t, testnet)  =
  let
    (kind, content) = case t of
      Address addr -> ("addr", viewAddr addr)
      TransactionsForAddress tfa -> ("addr", viewTfA tfa)
      OperationsForAddress ofa -> ("addr", viewOfA ofa)
      EffectsForAddress efa -> ("addr", viewEfA efa)
      Transaction txn -> ("txn", viewTxn txn)
      OperationsForTransaction oft -> ("txn", viewOfT oft)
      EffectsForTransaction eft -> ("txn", viewEfT eft)
      Operation op -> ("op", viewOp op)
      EffectsForOperation efo -> ("op", viewEfO efo)
      Ledger led -> ("led", viewLed led)
      TransactionsForLedger tfl -> ("led", viewTfL tfl)
      OperationsForLedger ofl -> ("led", viewOfL ofl)
      EffectsForLedger efl -> ("led", viewEfL efl)
      Empty -> ("empty", text "")
      Loading -> ("loading", loading)
      Errored err -> ("errored", text <| errorFormat err)
  in
    div
      [ class <| "box thing " ++ kind ++ (if testnet then " testnet" else "")
      ] [ content ]


viewAddr : Addr -> Html GlobalAction
viewAddr addr =
  div []
    [ h1 [ class "title", onClick SurfHere ]
      [ text "Address "
      , span [ title addr.id, hashcolor addr.id ] [ text <| wrap addr.id ]
      ]
    , table []
      [ tr []
        [ th [] [ text "id" ]
        , td [] [ text <| String.toUpper addr.id ]
        ]
      , tr []
        [ th [] [ text "balances" ]
        , td []
          [ table [ class "table is-hoverable is-fullwidth" ]
            [ thead []
              [ tr []
                [ th [] [ text "asset" ]
                , th [] [ text "amount" ]
                ]
              ]
            , tbody []
              <| List.map (balanceRow .balance) addr.balances
            ]
          ]
        ]
      , let
          balances = List.filter (.asset >> .native >> not) addr.balances
        in
          if List.length balances == 0
          then text ""
          else tr []
            [ th [] [ text "trust lines" ]
            , td []
              [ table [ class "table is-hoverable is-fullwidth" ]
                [ thead []
                  [ tr []
                    [ th [] [ text "name" ]
                    , th [] [ text "max trust" ]
                    ]
                  ]
                , tbody []
                  <| List.map (balanceRow <| .limit >> limitwrap) balances
                ]
              ]
            ]
      , tr []
        [ th [] [ text "transactions" ]
        , td []
          [ a
            [ onClick (NavigateTo <| "/txnsforaddr/" ++ addr.id) ]
            [ text "list all" ] ]
        ]
      , tr []
        [ th [] [ text "operations" ]
        , td []
          [ a
            [ onClick (NavigateTo <| "/opsforaddr/" ++ addr.id) ]
            [ text "list all" ] ]
        ]
      , tr []
        [ th [] [ text "effects" ]
        , td []
          [ a
            [ onClick (NavigateTo <| "/effsforaddr/" ++ addr.id) ]
            [ text "list all" ]
          ]
        ]
      , tr []
        [ th [ class "wrappable" ] [ text "home_domain" ]
        , td [] [ text addr.home_domain ]
        ]
      , tr []
        [ th [ class "wrappable" ] [ text "inflation_destination" ]
        , td []
          [ if addr.inflation_destination /= ""
            then addrlink addr.inflation_destination
            else text ""
          ]
        ]
      , tr []
        [ th [] [ text "data" ]
        , td []
          [ table []
            <| List.map
              ( \(k, v) ->
                tr [] [ td [] [ text k ] , td [] [ text v ] ]
              )
            <| Dict.toList addr.data
          ]
        ]
      ]
    ]

balanceRow : (Balance -> String) -> Balance -> Html GlobalAction
balanceRow getter balance =
  tr []
    [ td [ class "singleline" ] [ viewAsset balance.asset ]
    , td [ title <| getter balance, class "wrappable" ] [ text <| getter balance ]
    ]

viewTfA : TfA -> Html GlobalAction
viewTfA tfa =
  div []
    [ h1 [ class "title is-3", onClick SurfHere ]
      [ text "Transactions for "
      , span [ title tfa.addr, hashcolor tfa.addr ] [ text <| wrap tfa.addr ]
      ]
    , table []
      [ shortTxnHeader
      , tbody []
        <| List.map shortTxnRow tfa.transactions
      ]
    ]

viewEfA : EfA -> Html GlobalAction
viewEfA efa = viewEffList efa.effects efa.addr

shortTxnHeader : Html msg
shortTxnHeader =
  thead []
    [ tr []
      [ th [] [ text "hash" ]
      , th [] [ text "time" ]
      , th [] [ text "source" ]
      , th [] [ text "ops" ]
      ]
    ]

shortTxnRow : Txn -> Html GlobalAction
shortTxnRow txn =
  tr []
    [ td [] [ txnlink txn.hash ]
    , td
      [ class "hideable"
      , title <| date txn.created_at
      ] [ text <| dateShort txn.created_at ]
    , td [] [ addrlink txn.source_account ]
    , td []
      [ a [ onClick (NavigateTo <| "/opsfortxn/" ++ txn.hash) ]
        [ text <| toString txn.operation_count ]
      ]
    ]

viewTxn : Txn -> Html GlobalAction
viewTxn txn =
  div []
    [ h1 [ class "title", onClick SurfHere ]
      [ text "Transaction "
      , span [ title txn.hash, hashcolor txn.hash ] [ text <| wrap txn.hash ]
      ]
    , table []
      [ tr []
        [ th [] [ text "hash" ]
        , td [] [ text <| txn.hash ]
        ]
      , tr []
        [ th [] [ text "ledger" ]
        , td [] [ ledlink txn.ledger ]
        ]
      , tr []
        [ th [] [ text "created_at" ]
        , td [] [ text <| date txn.created_at ]
        ]
      , tr []
        [ th [ class "wrappable" ] [ text "source_account" ]
        , td [] [ addrlink txn.source_account ]
        ]
      , tr []
        [ th [] [ text "ops" ]
        , td []
          [ a [ onClick (NavigateTo <| "/opsfortxn/" ++ txn.hash) ]
            [ span [ class "emphasis" ] [ text <| toString txn.operation_count ]
            , text " (click to browse)"
            ]
          ]
        ]
      , tr []
        [ th [] [ text "fee_paid" ]
        , td [] [ text <| toString txn.fee_paid ]
        ]
      , tr []
        [ th [] [ text "effects" ]
        , td []
          [ a
            [ onClick (NavigateTo <| "/effsfortxn/" ++ txn.hash) ]
            [ text "list all" ]
          ]
        ]
      ]
    ]

viewOfT : OfT -> Html GlobalAction
viewOfT oft =
  div []
    [ h1 [ class "title is-3", onClick SurfHere ]
      [ text "Operations for "
      , span [ title oft.hash, hashcolor oft.hash ] [ text <| wrap oft.hash ]
      ]
    , table []
      [ shortOpHeader
      , tbody []
        <| List.map shortOpRow oft.operations
      ]
    ]

viewOfA : OfA -> Html GlobalAction
viewOfA ofa =
  div []
    [ h1 [ class "title is-3", onClick SurfHere ]
      [ text "Operations for "
      , span [ title ofa.addr, hashcolor ofa.addr ] [ text <| wrap ofa.addr ]
      ]
    , table []
      [ shortOpHeader
      , tbody []
        <| List.map shortOpRow ofa.operations
      ]
    ]

viewEfT : EfT -> Html GlobalAction
viewEfT eft = viewEffList eft.effects eft.hash

shortOpHeader : Html msg
shortOpHeader =
  thead []
    [ tr []
      [ th [] [ text "id" ]
      , th [] [ text "type" ]
      , th [] [ text "source" ]
      , th [] [ text "txn" ]
      ]
    ]

shortOpRow : Op -> Html GlobalAction
shortOpRow op =
  tr []
    [ td [] [ oplink op.id ]
    , td [] [ text op.type_ ]
    , td [] [ addrlink op.source_account ]
    , td [] [ txnlink op.txn ]
    ]

viewOp : Op -> Html GlobalAction
viewOp op =
  let
    generalrows =
      [ tr []
        [ th [] [ text "id" ]
        , td [] [ text <| String.toLower op.id ]
        ]
      , tr []
        [ th [ class "wrappable" ] [ text "source_account" ]
        , td [] [ addrlink op.source_account ]
        ]
      , tr []
        [ th [] [ text "transaction" ]
        , td [] [ txnlink op.txn ]
        ]
      ]
    specificrows = opDataRows op.data
    effectrows =
      [ tr []
        [ th [] [ text "effects" ]
        , td []
          [ a
            [ onClick (NavigateTo <| "/effsforop/" ++ op.id) ]
            [ text "list all" ]
          ]
        ]
      ]
    rows = List.concat [ generalrows, specificrows, effectrows ]
  in div []
    [ h1 [ class "title", onClick SurfHere ]
      [ span [ class "emphasis" ] [ text op.type_ ]
      , span [] [ text " Operation" ]
      ]
    , table [] rows
    ]

viewEfO : EfO -> Html GlobalAction
viewEfO efo = viewEffList efo.effects efo.id

shortLedRow : Led -> Html GlobalAction
shortLedRow led =
  let seq = toString led.sequence
  in tr []
    [ td [] [ ledlink led.sequence ]
    , td
      [ class "hideable"
      , title <| date led.closed_at
      ] [ text <| time led.closed_at ]
    , td []
      [ a [ onClick (NavigateTo <| "/txnsforled/" ++ seq) ]
        [ text <| toString led.transaction_count ]
      ]
    , td []
      [ a [ onClick (NavigateTo <| "/opsforled/" ++ seq) ]
        [ text <| toString led.operation_count ]
      ]
    ]


viewLed : Led -> Html GlobalAction
viewLed led =
  let seq = toString led.sequence
  in div []
    [ h1 [ class "title", onClick SurfHere ]
      [ span [] [ text "Ledger " ]
      , span [ title seq, hashcolor seq ] [ text seq ]
      ]
    , table []
      [ tr []
        [ th [] [ text "hash" ]
        , td [ title led.hash, class "wrappable" ] [ text <| led.hash ]
        ]
      , tr []
        [ th [] [ text "previous" ]
        , td [] [ ledlink (led.sequence - 1) ]
        ]
      , tr []
        [ th [] [ text "next" ]
        , td [] [ ledlink (led.sequence + 1) ]
        ]
      , tr []
        [ th [] [ text "closed_at" ]
        , td [] [ text <| date led.closed_at ]
        ]
      , tr []
        [ th [ class "wrappable" ] [ text "transaction_count" ]
        , td []
          [ a [ onClick (NavigateTo <| "/txnsforled/" ++ seq) ]
            [ span [ class "emphasis" ] [ text <| toString led.transaction_count ]
            , text " (click to browse)"
            ]
          ]
        ]
      , tr []
        [ th [ class "wrappable" ] [ text "operation_count" ]
        , td []
          [ a [ onClick (NavigateTo <| "/opsforled/" ++ seq) ]
            [ span [ class "emphasis" ] [ text <| toString led.operation_count ]
            , text " (click to browse)"
            ]
          ]
        ]
      , tr []
        [ th [] [ text "effects" ]
        , td []
          [ a
            [ onClick (NavigateTo <| "/effsforled/" ++ seq) ]
            [ text "list all" ]
          ]
        ]
      , tr []
        [ th [] [ text "total_coins" ]
        , td [] [ text led.network.total_coins ]
        ]
      , tr []
        [ th [] [ text "fee_pool" ]
        , td [] [ text led.network.fee_pool ]
        ]
      , tr []
        [ th [] [ text "base_fee" ]
        , td [] [ text <| toString led.network.base_fee ]
        ]
      , tr []
        [ th [] [ text "base_reserve" ]
        , td [] [ text led.network.base_reserve ]
        ]
      , tr []
        [ th [ class "wrappable" ] [ text "max_tx_set_size" ]
        , td [] [ text <| toString led.network.max_tx_set_size ]
        ]
      , tr []
        [ th [ class "wrappable" ] [ text "protocol_version" ]
        , td [] [ text <| toString led.network.protocol_version ]
        ]
      ]
    ]


viewTfL : TfL -> Html GlobalAction
viewTfL tfl =
  let seq = toString tfl.sequence
  in div []
    [ h1 [ class "title is-3", onClick SurfHere ]
      [ span [] [ text "Transactions for " ]
      , span [ title seq, hashcolor seq ] [ text seq ]
      ]
    , table []
      [ shortTxnHeader
      , tbody []
        <| List.map shortTxnRow tfl.transactions
      ]
    ]

viewOfL : OfL -> Html GlobalAction
viewOfL ofl =
  let seq = toString ofl.sequence
  in div []
    [ h1 [ class "title is-3", onClick SurfHere ]
      [ span [] [ text "Operations for " ]
      , span [ title seq, hashcolor seq ] [ text seq ]
      ]
    , table []
      [ shortOpHeader
      , tbody []
        <| List.map shortOpRow ofl.operations
      ]
    ]

viewEfL : EfL -> Html GlobalAction
viewEfL efl = viewEffList efl.effects (toString efl.sequence)


viewEffList : List Eff -> String -> Html GlobalAction
viewEffList effects parent_id =
  div []
    [ h1 [ class "title is-3", onClick SurfHere ]
      [ text "Effects for "
      , span [ title parent_id, hashcolor parent_id ] [ text <| wrap parent_id ]
      ]
    , table []
      [ shortEffHeader
      , tbody []
        <| List.map shortEffRow effects
      ]
    ]

shortEffHeader : Html msg
shortEffHeader =
  thead []
    [ tr []
      [ th [] [ text "account" ]
      , th [] [ text "type" ]
      , th [ colspan 2 ] [ text "info" ]
      ]
    ]

shortEffRow : Eff -> Html GlobalAction
shortEffRow eff =
  tr [] <|
    List.append
      [ td [] [ addrlink eff.account ]
      , td [] [ text eff.type_ ]
      ]
      ( effDataRows eff.effData )
