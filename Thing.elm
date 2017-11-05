module Thing exposing (..)

import Html exposing
  ( Html, Attribute, text
  , h1, h2, div, textarea, button, p, a
  , table, tbody, thead, tr, th, td
  , input, select, option, header
  , span, section, img, label
  )
import Html.Attributes exposing (class, title, attribute)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Http
import Task
import Json.Decode as J
import Route exposing (..)

import Operations exposing (..)
import Asset exposing (..)
import Helpers exposing (..)


routes = router
  [ route (Address << \s -> { defaultAddr | id = s })
    <| static "addr" </> string
  , route (TransactionsForAddress << \s -> { defaultTfA | addr = s })
    <| static "txnsforaddr" </> string
  , route (OperationsForAddress << \s -> { defaultOfA | addr = s })
    <| static "opsforaddr" </> string
  , route (Transaction << \s -> { defaultTxn | hash = s })
    <| static "txn" </> string
  , route (OperationsForTransaction << \s -> { defaultOfT | hash = s })
    <| static "opsfortxn" </> string
  , route (Operation << \s -> { defaultOp | id = s })
    <| static "op" </> string
  , route (Ledger << \s -> { defaultLed | sequence = s })
    <| static "led" </> int
  , route (OperationsForLedger << \s -> { defaultOfL | sequence = s })
    <| static "opsforled" </> int
  , route (TransactionsForLedger << \s -> { defaultTfL | sequence = s })
    <| static "txnsforled" </> int
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
  | Transaction Txn
  | OperationsForTransaction OfT
  | Operation Op
  | Ledger Led
  | OperationsForLedger OfL
  | TransactionsForLedger TfL
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
      "/accounts/" ++ tfa.addr ++ "/transactions?order=desc&limit=23"
    OperationsForAddress ofa ->
      "/accounts/" ++ ofa.addr ++ "/operations?order=desc&limit=23"
    Transaction txn -> "/transactions/" ++ txn.hash
    OperationsForTransaction oft ->
      "/transactions/" ++ oft.hash ++ "/operations"
    Operation op -> "/operations/" ++ op.id
    Ledger led -> "/ledgers/" ++ (toString led.sequence)
    OperationsForLedger ofl ->
      "/ledgers/" ++ (toString ofl.sequence) ++ "/operations"
    TransactionsForLedger tfl ->
      "/ledgers/" ++ (toString tfl.sequence) ++ "/transactions"
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
    Transaction _ -> txnDecoder |> J.map Transaction
    OperationsForTransaction oft ->
      oftDecoder
        |> J.map (\f -> { f | hash = oft.hash })
        |> J.map OperationsForTransaction
    Operation _ -> opDecoder |> J.map Operation
    Ledger _ -> ledDecoder |> J.map Ledger
    OperationsForLedger ofl ->
      oflDecoder
        |> J.map (\f -> { f | sequence = ofl.sequence })
        |> J.map OperationsForLedger
    TransactionsForLedger tfl ->
      tflDecoder
        |> J.map (\f -> { f | sequence = tfl.sequence })
        |> J.map TransactionsForLedger
    Errored err -> J.null (Errored err)
    Loading -> J.null Loading
    Empty -> J.null Empty

type alias Addr =
  { id : String
  , balances : List Balance
  }

defaultAddr = Addr "" []

addrDecoder : J.Decoder Addr
addrDecoder =
  J.map2 Addr
    ( J.field "id" J.string )
    ( J.field "balances" <| J.list balanceDecoder )

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

type alias TfA =
  { addr : String
  , transactions : List Txn
  }

defaultTfA = TfA "" []

tfaDecoder : J.Decoder TfA
tfaDecoder =
  J.map2 TfA
    ( J.succeed "" )
    ( J.at ["_embedded", "records"] (J.list txnDecoder) )

type alias OfA =
  { addr : String
  , operations : List Op
  }

defaultOfA = OfA "" []

ofaDecoder : J.Decoder OfA
ofaDecoder =
  J.map2 OfA
    ( J.succeed "" )
    ( J.at ["_embedded", "records"] (J.list opDecoder) )

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

type alias OfT =
  { hash : String
  , operations : List Op
  }

defaultOfT = OfT "" []

oftDecoder : J.Decoder OfT
oftDecoder =
  J.map2 OfT
    ( J.succeed "" )
    ( J.at ["_embedded", "records"] (J.list opDecoder) )

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


type alias TfL =
  { sequence : Int
  , transactions : List Txn
  }

defaultTfL = TfL 0 []

tflDecoder : J.Decoder TfL
tflDecoder =
  J.map2 TfL
    ( J.succeed 0 )
    ( J.at ["_embedded", "records"] (J.list txnDecoder) )


type alias OfL =
  { sequence : Int
  , operations : List Op
  }

defaultOfL = OfL 0 []

oflDecoder : J.Decoder OfL
oflDecoder =
  J.map2 OfL
    ( J.succeed 0 )
    ( J.at ["_embedded", "records"] (J.list opDecoder) )


viewThing : (Thing, Bool) -> Html GlobalAction
viewThing (t, testnet)  =
  let
    (kind, content) = case t of
      Address addr -> ("addr", viewAddr addr)
      TransactionsForAddress tfa -> ("addr", viewTfA tfa)
      OperationsForAddress ofa -> ("addr", viewOfA ofa)
      Transaction txn -> ("txn", viewTxn txn)
      OperationsForTransaction oft -> ("txn", viewOfT oft)
      Operation op -> ("op", viewOp op)
      Ledger led -> ("led", viewLed led)
      TransactionsForLedger tfl -> ("led", viewTfL tfl)
      OperationsForLedger ofl -> ("led", viewOfL ofl)
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
        , td [] [ text <| String.toLower addr.id ]
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
                  <| List.map (balanceRow .limit) balances
                ]
              ]
            ]
      , tr []
        [ th [] [ text "transactions" ]
        , td []
          [ a
            [ onClick (NavigateTo <| "/txnsforaddr/" ++ addr.id) ]
            [ text "view last 23" ] ]
        ]
      , tr []
        [ th [] [ text "operations" ]
        , td []
          [ a
            [ onClick (NavigateTo <| "/opsforaddr/" ++ addr.id) ]
            [ text "view last 23" ] ]
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
    rows = List.concat [ generalrows, specificrows ]
  in div []
    [ h1 [ class "title", onClick SurfHere ]
      [ span [ class "emphasis" ] [ text op.type_ ]
      , span [] [ text " Operation" ]
      ]
    , table [] rows
    ]

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
