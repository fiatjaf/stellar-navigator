module Thing exposing (..)

import Html exposing
  ( Html, text
  , h1, h2, div, textarea, button, p, a
  , table, tbody, thead, tr, th, td
  , input, select, option, header, nav
  , span, section, nav, img, label
  )
import Http
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Task
import Json.Decode as J
import Route exposing (..)


type ThingRoute
  = AddrRoute String
  | TxnsForAddrRoute String
  | TxnRoute String
  | OpsForTxnRoute String
  | OpRoute String
  | EmptyRoute

routes = router
  [ AddrRoute := static "addr" </> string
  , TxnsForAddrRoute := static "txnsforaddr" </> string
  , TxnRoute := static "txn" </> string
  , OpsForTxnRoute := static "opsfortxn" </> string
  , OpRoute := static "op" </> string
  ] 

match : String -> ThingRoute
match = Route.match routes >> Maybe.withDefault EmptyRoute

base = "https://horizon-testnet.stellar.org"

fetch : String -> (Result Http.Error Thing -> msg) -> Cmd msg
fetch pathname hmsg =
  let 
    (decoder, url) = case match pathname of
      AddrRoute addr ->
        ( addrDecoder |> J.map Address, base ++ "/accounts/" ++ addr )
      TxnsForAddrRoute addr ->
        ( J.at ["_embedded", "records"] (J.list txnDecoder) |> J.map TransactionsForAddress
        , base ++ "/accounts/" ++ addr ++ "/transactions?order=desc&limit=15"
        )
      TxnRoute hash ->
        ( txnDecoder |> J.map Transaction, base ++ "/transactions/" ++ hash )
      OpsForTxnRoute hash ->
        ( J.at ["_embedded", "records"] (J.list opDecoder) |> J.map OperationsForTransaction
        , base ++ "/transactions/" ++ hash ++ "/operations"
        )
      OpRoute id ->
        ( opDecoder |> J.map Operation, base ++ "/operations/" ++ id )
      EmptyRoute ->
        (J.null Empty, "")
  in
    if url == ""
    then hmsg (Ok Empty)
      |> Task.succeed
      |> Task.perform identity 
    else Http.get url decoder |> Http.send hmsg


type Thing
  = Address Addr
  | TransactionsForAddress (List Txn)
  | Transaction Txn
  | OperationsForTransaction (List Op)
  | Operation Op
  | Empty
  | Errored Http.Error

type alias Addr =
  { id : String
  , balances : List Balance
  }

addrDecoder : J.Decoder Addr
addrDecoder =
  J.map2 Addr
    ( J.field "id" J.string )
    ( J.field "balances" <| J.list balanceDecoder )

type alias Balance =
  { asset_code : String
  , asset_issuer : String
  , balance : String
  , limit : String
  }

balanceDecoder : J.Decoder Balance
balanceDecoder =
  J.map4 Balance
    ( J.oneOf
      [ J.field "asset_code" J.string
      , J.succeed "Lumens"
      ]
    )
    ( J.oneOf
      [ J.field "asset_issuer" J.string
      , J.succeed ""
      ]
    )
    ( J.field "balance" J.string )
    ( J.oneOf
      [ J.field "limit" J.string
      , J.succeed ""
      ]
    )

type alias Txn =
  { hash : String
  , ledger : Int
  , created_at : String
  , source_account : String
  , operation_count : Int
  , fee_paid : Int
  }

txnDecoder : J.Decoder Txn
txnDecoder =
  J.map6 Txn
    ( J.field "hash" J.string )
    ( J.field "ledger" J.int )
    ( J.field "created_at" J.string )
    ( J.field "source_account" J.string )
    ( J.field "operation_count" J.int )
    ( J.field "fee_paid" J.int )

type alias Op =
  { id : String
  , source_account : String
  , type_ : String
  }

opDecoder : J.Decoder Op
opDecoder =
  J.map3 Op
    ( J.field "id" J.string )
    ( J.field "source_account" J.string )
    ( J.field "type" J.string )


viewThing : (String -> msg) -> Thing -> Html msg
viewThing nav t =
  case t of
    Address addr -> div [ class "box thing addr" ] [ viewAddr nav addr ]
    TransactionsForAddress txns -> div [ class "box thing addr" ] [ viewTxnsForAddr nav txns ]
    Transaction txn -> div [ class "box thing txn" ] [ viewTxn nav txn ]
    OperationsForTransaction ops -> div [ class "box thing txn" ] [ viewOpsForTxn nav ops ]
    Operation op -> div [ class "box thing op" ] [ viewOp nav op ]
    Empty -> div [ class "box thing empty" ] []
    Errored err -> div [ class "box thing errored" ] [ text <| errorFormat err ]

viewAddr : (String -> msg) -> Addr -> Html msg
viewAddr nav addr =
  div []
    [ h1 [ class "title", title addr.id ] [ text <| "Address " ++ (wrap addr.id) ]
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
                [ th [] [ text "name" ]
                , th [] [ text "issuer" ]
                , th [] [ text "amount" ]
                ]
              ]
            , tbody []
              <| List.map (assetRow nav .balance) addr.balances
            ]
          ]
        ]
      , let
          assets = List.filter (.asset_issuer >> ((/=) "")) addr.balances
        in
          if List.length assets == 0
          then text ""
          else tr []
            [ th [] [ text "trust lines" ]
            , td []
              [ table [ class "table is-hoverable is-fullwidth" ]
                [ thead []
                  [ tr []
                    [ th [] [ text "name" ]
                    , th [] [ text "issuer" ]
                    , th [] [ text "max trust" ]
                    ]
                  ]
                , tbody []
                  <| List.map (assetRow nav .limit) assets
                ]
              ]
            ]
      , tr []
        [ td [ rowspan 2 ]
          [ a [ onClick (nav <| "/txnsforaddr/" ++ addr.id) ] [ text "last transactions" ] ]
        ]
      ]
    ]

assetRow : (String -> msg) -> (Balance -> String) -> Balance -> Html msg
assetRow nav getter asset =
  tr []
    [ td [] [ text asset.asset_code ]
    , td []
      [ a [ onClick (nav <| "/addr/" ++ asset.asset_issuer ) ]
        [ text <| wrap asset.asset_issuer
        ]
      ]
    , td [] [ text <| getter asset ]
    ]

viewTxnsForAddr : (String -> msg) -> List Txn -> Html msg
viewTxnsForAddr nav txns =
  div []
    [ h1 [ class "title is-4" ] [ text "Transactions for Address" ]
    , table []
      [ thead []
        [ tr []
          [ th [] [ text "hash" ]
          , th [] [ text "time" ]
          , th [] [ text "source_account" ]
          ]
        ]
      , tbody []
        <| List.map (shortTxnRow nav) txns
      ]
    ]

shortTxnRow : (String -> msg) -> Txn -> Html msg
shortTxnRow nav txn =
  tr []
    [ td []
      [ a [ onClick (nav <| "/txn/" ++ txn.hash) ] [ text <| wrap txn.hash ]
      ]
    , td [] [ text txn.created_at ]
    , td []
      [ a [ onClick (nav <| "/addr/" ++ txn.source_account) ]
        [ text <| wrap txn.source_account
        ]
      ]
    ]

viewTxn : (String -> msg) -> Txn -> Html msg
viewTxn nav txn =
  div []
    [ h1 [ class "title" ] [ text <| "Transaction " ++ (wrap txn.hash) ]
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
        , td [] [ text txn.created_at ]
        ]
      , tr []
        [ th [] [ text "source_account" ]
        , td []
          [ a
            [ onClick (nav <| "/addr/" ++ txn.source_account)
            ] [ text <| wrap txn.source_account ]
          ]
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
        , td [] [ text <| toString txn.fee_paid ]
        ]
      ]
    ]

viewOpsForTxn : (String -> msg) -> List Op -> Html msg
viewOpsForTxn nav ops =
  div []
    [ h1 [ class "title is-4" ] [ text <| "Operations for Transaction" ]
    , table []
      [ thead []
        [ tr []
          [ th [] [ text "id" ]
          , th [] [ text "type" ]
          , th [] [ text "source_account" ]
          ]
        ]
      , tbody []
        <| List.map (shortOpRow nav) ops
      ]
    ]

shortOpRow : (String -> msg) -> Op -> Html msg
shortOpRow nav op =
  tr []
    [ td []
      [ a [ onClick (nav <| "/op/" ++ op.id) ] [ text <| wrap op.id ]
      ]
    , td [] [ text op.type_ ]
    , td []
      [ a [ onClick (nav <| "/addr/" ++ op.source_account) ] [ text <| wrap op.source_account ]
      ]
    ]

viewOp : (String -> msg) -> Op -> Html msg
viewOp nav op =
  div []
    [ h1 [ class "title" ] [ text <| "Operation " ++ (wrap op.id) ]
    , table []
      [ tr []
        [ th [] [ text "source_account" ]
        , td []
          [ a [ onClick (nav <| "/addr/" ++ op.source_account) ]
            [ text <| wrap op.source_account
            ]
          ]
        ]
      , tr []
        [ th [] [ text "type" ]
        , td [] [ text op.type_ ]
        ]
      ]
    ]


wrap : String -> String
wrap str =
  if str == ""
  then ""
  else (String.left 3 str) ++ "..." ++ (String.right 4 str)
    |> String.toLower

errorFormat : Http.Error -> String
errorFormat err =
  case err of
    Http.BadUrl u -> "bad url " ++ u
    Http.Timeout -> "timeout"
    Http.NetworkError -> "network error"
    Http.BadStatus resp ->
      resp.url ++ " returned " ++ (toString resp.status.code) ++ ": " ++ resp.body
    Http.BadPayload x y -> "bad payload (" ++ x ++ ")"
