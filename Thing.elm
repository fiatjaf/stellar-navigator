module Thing exposing (..)

import Html exposing
  ( Html, Attribute, text
  , h1, h2, div, textarea, button, p, a
  , table, tbody, thead, tr, th, td
  , input, select, option, header, nav
  , span, section, nav, img, label
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
  , route (Transaction << \s -> { defaultTxn | hash = s })
    <| static "txn" </> string
  , route (OperationsForTransaction << \s -> { defaultOfT | hash = s })
    <| static "opsfortxn" </> string
  , route (Operation << \s -> { defaultOp | id = s })
    <| static "op" </> string
  , route (Ledger << \s -> { defaultLed | sequence = s })
    <| static "led" </> int
  ] 

match : String -> Thing
match = Route.match routes >> Maybe.withDefault Empty

fetch : String -> String -> (Result Http.Error Thing -> msg) -> Cmd msg
fetch base pathname hmsg =
  let
    thing = match pathname
    url = base ++ (thingUrl thing)
    decoder = thingDecoder thing
  in
    if url == ""
    then hmsg (Ok Empty)
      |> Task.succeed
      |> Task.perform identity 
    else Http.get url decoder |> Http.send hmsg


type Thing
  = Address Addr
  | TransactionsForAddress TfA
  | Transaction Txn
  | OperationsForTransaction OfT
  | Operation Op
  | Ledger Led
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
      "/accounts/" ++ tfa.addr ++ "/transactions?order=desc&limit=15"
    Transaction txn -> "/transactions/" ++ txn.hash
    OperationsForTransaction oft ->
      "/transactions/" ++ oft.hash ++ "/operations"
    Operation op -> "/operations/" ++ op.id
    Ledger led -> "/ledgers/" ++ (toString led.sequence)
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
    Transaction _ -> txnDecoder |> J.map Transaction
    OperationsForTransaction oft ->
      oftDecoder
        |> J.map (\f -> { f | hash = oft.hash })
        |> J.map OperationsForTransaction
    Operation _ -> opDecoder |> J.map Operation
    Ledger _ -> ledDecoder |> J.map Ledger
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
  , data : OpData
  , txn : String
  }

defaultOp = Op "" "" "" None ""

opDecoder : J.Decoder Op
opDecoder =
  J.map5 Op
    ( J.field "id" J.string )
    ( J.field "source_account" J.string )
    ( J.field "type" J.string )
    ( opDataDecoder )
    ( J.at [ "_links", "transaction", "href" ] J.string
      |> J.map (String.split "/" >> List.reverse >> List.head >> Maybe.withDefault "")
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


viewThing : msg -> (String -> msg) -> (Thing, Bool) -> Html msg
viewThing surf nav (t, testnet)  =
  let
    (kind, content) = case t of
      Address addr -> ("addr", viewAddr surf nav addr)
      TransactionsForAddress tfa -> ("addr", viewTfA surf nav tfa)
      Transaction txn -> ("txn", viewTxn surf nav txn)
      OperationsForTransaction oft -> ("txn", viewOfT surf nav oft)
      Operation op -> ("op", viewOp surf nav op)
      Ledger led -> ("led", viewLed surf nav led)
      Empty -> ("empty", text "")
      Loading -> ("loading", loading)
      Errored err -> ("errored", text <| errorFormat err)
  in
    div
      [ class <| "box thing " ++ kind ++ (if testnet then " testnet" else "")
      ] [ content ]


viewAddr : msg -> (String -> msg) -> Addr -> Html msg
viewAddr surf nav addr =
  div []
    [ h1
      [ class "title"
      , title addr.id
      , hashcolor addr.id
      , onClick surf
      ] [ text <| "Address " ++ (wrap addr.id) ]
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
              <| List.map (balanceRow nav .balance) addr.balances
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
                  <| List.map (balanceRow nav .limit) balances
                ]
              ]
            ]
      , tr []
        [ th [] [ text "transactions" ]
        , td []
          [ a [ onClick (nav <| "/txnsforaddr/" ++ addr.id) ] [ text "last 15" ] ]
        ]
      ]
    ]

balanceRow : (String -> msg) -> (Balance -> String) -> Balance -> Html msg
balanceRow nav getter balance =
  tr []
    [ td [ class "singleline" ] [ viewAsset nav balance.asset ]
    , td [ title <| getter balance, class "wrappable" ] [ text <| getter balance ]
    ]

viewTfA : msg -> (String -> msg) -> TfA -> Html msg
viewTfA surf nav tfa =
  div []
    [ h1
      [ class "title is-4"
      , hashcolor tfa.addr
      , onClick surf
      ] [ text <| "Transactions for Address " ++ (wrap tfa.addr) ]
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
        <| List.map (shortTxnRow nav) tfa.transactions
      ]
    ]

shortTxnRow : (String -> msg) -> Txn -> Html msg
shortTxnRow nav txn =
  tr []
    [ td [] [ txnlink nav txn.hash ]
    , td
      [ class "hideable"
      , title <| date txn.created_at
      ] [ text <| dateShort txn.created_at ]
    , td [] [ addrlink nav txn.source_account ]
    , td [] [ text <| toString txn.operation_count ]
    ]

viewTxn : msg -> (String -> msg) -> Txn -> Html msg
viewTxn surf nav txn =
  div []
    [ h1
      [ class "title"
      , hashcolor txn.hash
      , onClick surf
      ] [ text <| "Transaction " ++ (wrap txn.hash) ]
    , table []
      [ tr []
        [ th [] [ text "hash" ]
        , td [] [ text <| txn.hash ]
        ]
      , tr []
        [ th [] [ text "ledger" ]
        , td [] [ ledlink nav txn.ledger ]
        ]
      , tr []
        [ th [] [ text "created_at" ]
        , td [] [ text <| date txn.created_at ]
        ]
      , tr []
        [ th [] [ text "source_account" ]
        , td [] [ addrlink nav txn.source_account ]
        ]
      , tr []
        [ th [] [ text "ops" ]
        , td []
          [ a
            [ onClick (nav <| "/opsfortxn/" ++ txn.hash)
            ] [ text <| toString txn.operation_count ]
        ]
      ]
      , tr []
        [ th [] [ text "fee_paid" ]
        , td [ class "emphasis" ] [ text <| toString txn.fee_paid ]
        ]
      ]
    ]

viewOfT : msg -> (String -> msg) -> OfT -> Html msg
viewOfT surf nav oft =
  div []
    [ h1
      [ class "title is-4"
      , hashcolor oft.hash
      , onClick surf
      ] [ text <| "Operations for Transaction " ++ (wrap oft.hash) ]
    , table []
      [ thead []
        [ tr []
          [ th [] [ text "id" ]
          , th [] [ text "type" ]
          , th [] [ text "source" ]
          ]
        ]
      , tbody []
        <| List.map (shortOpRow nav) oft.operations
      ]
    ]

shortOpRow : (String -> msg) -> Op -> Html msg
shortOpRow nav op =
  tr []
    [ td [] [ oplink nav op.id ]
    , td [] [ text op.type_ ]
    , td [] [ addrlink nav op.source_account ]
    ]

viewOp : msg -> (String -> msg) -> Op -> Html msg
viewOp surf nav op =
  div []
    [ h1
      [ class "title"
      , hashcolor op.id
      , onClick surf
      ]
      [ span [ class "emphasis" ] [ text op.type_ ]
      , text " operation"
      ]
    , table []
      <| List.concat
        [ [ tr []
            [ th [] [ text "id" ]
            , td [] [ text <| String.toLower op.id ]
            ]
          , tr []
            [ th [] [ text "source_account" ]
            , td [] [ addrlink nav op.source_account ]
            ]
          , tr []
            [ th [] [ text "transaction" ]
            , td [] [ txnlink nav op.txn ]
            ]
          ]
        , ( opDataRows nav op.data )
        ]
    ]

shortLedRow : (String -> msg) -> Led -> Html msg
shortLedRow nav led =
  tr []
    [ td [] [ ledlink nav led.sequence ]
    , td
      [ class "hideable"
      , title <| date led.closed_at
      ] [ text <| time led.closed_at ]
    , td [] [ text <| toString led.transaction_count ]
    , td [] [ text <| toString led.operation_count ]
    ]


viewLed : msg -> (String -> msg) -> Led -> Html msg
viewLed surf nav led =
  div []
    [ h1
      [ class "title"
      , hashcolor <| toString led.sequence
      , onClick surf
      ] [ text <| "Ledger " ++ (toString led.sequence) ]
    , table []
      [ tr []
        [ th [] [ text "hash" ]
        , td [] [ text <| led.hash ]
        ]
      , tr []
        [ th [] [ text "previous" ]
        , td [] [ ledlink nav (led.sequence - 1) ]
        ]
      , tr []
        [ th [] [ text "next" ]
        , td [] [ ledlink nav (led.sequence + 1) ]
        ]
      , tr []
        [ th [] [ text "closed_at" ]
        , td [] [ text <| date led.closed_at ]
        ]
      , tr []
        [ th [] [ text "transaction_count" ]
        , td []
          [ a
            [ onClick (nav <| "/txnsforled/" ++ (toString led))
            ] [ text <| toString led.transaction_count ]
          ]
        ]
      , tr []
        [ th [] [ text "operation_count" ]
        , td []
          [ a
            [ onClick (nav <| "/opsforled/" ++ (toString led))
            ] [ text <| toString led.operation_count ]
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
        [ th [] [ text "max_tx_set_size" ]
        , td [] [ text <| toString led.network.max_tx_set_size ]
        ]
      , tr []
        [ th [] [ text "protocol_version" ]
        , td [] [ text <| toString led.network.protocol_version ]
        ]
      ]
    ]
