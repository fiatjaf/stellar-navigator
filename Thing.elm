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
  }

defaultOp = Op "" "" "" None

opDecoder : J.Decoder Op
opDecoder =
  J.map4 Op
    ( J.field "id" J.string )
    ( J.field "source_account" J.string )
    ( J.field "type" J.string )
    ( opDataDecoder )


viewThing : msg -> (String -> msg) -> (Thing, Bool) -> Html msg
viewThing surf nav (t, testnet)  =
  let
    (kind, content) = case t of
      Address addr -> ("addr", viewAddr nav addr)
      TransactionsForAddress tfa -> ("addr", viewTfA nav tfa)
      Transaction txn -> ("txn", viewTxn nav txn)
      OperationsForTransaction oft -> ("txn", viewOfT nav oft)
      Operation op -> ("op", viewOp nav op)
      Empty -> ("empty", text "")
      Loading -> ("loading", loading)
      Errored err -> ("errored", text <| errorFormat err)
  in
    div
      [ onClick surf
      , class <| "box thing " ++ kind ++ (if testnet then " testnet" else "")
      ] [ content ]


viewAddr : (String -> msg) -> Addr -> Html msg
viewAddr nav addr =
  div []
    [ h1
      [ class "title"
      , title addr.id
      , hashcolor addr.id
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
    [ td [] [ viewAsset nav balance.asset ]
    , td [] [ text <| getter balance ]
    ]

viewTfA : (String -> msg) -> TfA -> Html msg
viewTfA nav tfa =
  div []
    [ h1
      [ class "title is-4"
      , hashcolor tfa.addr
      ] [ text <| "Transactions for Address " ++ (wrap tfa.addr) ]
    , table []
      [ thead []
        [ tr []
          [ th [] [ text "hash" ]
          , th [] [ text "time" ]
          , th [] [ text "source_account" ]
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
    , td [] [ text <| date txn.created_at ]
    , td [] [ addrlink nav txn.source_account ]
    ]

viewTxn : (String -> msg) -> Txn -> Html msg
viewTxn nav txn =
  div []
    [ h1
      [ class "title"
      , hashcolor txn.hash
      ] [ text <| "Transaction " ++ (wrap txn.hash) ]
    , table []
      [ tr []
        [ th [] [ text "hash" ]
        , td [] [ text <| txn.hash ]
        ]
      , tr []
        [ th [] [ text "ledger" ]
        , td [] [ text <| toString txn.ledger ]
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
        [ th [] [ text "operations" ]
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

viewOfT : (String -> msg) -> OfT -> Html msg
viewOfT nav oft =
  div []
    [ h1
      [ class "title is-4"
      , hashcolor oft.hash
      ] [ text <| "Operations for Transaction " ++ (wrap oft.hash) ]
    , table []
      [ thead []
        [ tr []
          [ th [] [ text "id" ]
          , th [] [ text "type" ]
          , th [] [ text "source_account" ]
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

viewOp : (String -> msg) -> Op -> Html msg
viewOp nav op =
  div []
    [ h1
      [ class "title"
      , hashcolor op.id
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
          ]
        , ( opDataRows nav op.data )
        ]
    ]

