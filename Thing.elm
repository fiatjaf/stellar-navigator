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
  | TxnRoute String
  | OpRoute String
  | EmptyRoute

addr = AddrRoute := static "addr" </> string
txn = TxnRoute := static "txn" </> string
op = OpRoute := static "op" </> string
routes = router [addr, txn, op] 

match : String -> ThingRoute
match = Route.match routes >> Maybe.withDefault EmptyRoute

base = "https://horizon-testnet.stellar.org"

fetch : String -> (Result Http.Error Thing -> msg) -> Cmd msg
fetch pathname hmsg =
  let 
    url = case match pathname of
      AddrRoute addr -> base ++ "/accounts/" ++ addr
      TxnRoute hash -> base ++ "/transactions/" ++ hash
      OpRoute id -> base ++ "/operations/" ++ id
      EmptyRoute -> ""
  in
    if url == ""
    then hmsg (Ok Empty)
      |> Task.succeed
      |> Task.perform identity 
    else Http.get url thingDecoder |> Http.send hmsg


type Thing
  = Address Addr
  | Transaction Txn
  | Operation Op
  | Empty
  | Errored Http.Error

thingDecoder : J.Decoder Thing
thingDecoder =
  J.oneOf
    [ addrDecoder |> J.map Address
    , txnDecoder |> J.map Transaction
    , opDecoder |> J.map Operation
    , J.null Empty
    ]

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
  { asset_type : String
  , asset_name : String
  , balance : String
  }

balanceDecoder : J.Decoder Balance
balanceDecoder =
  J.map3 Balance
    ( J.field "asset_type" J.string )
    ( J.oneOf
      [ J.field "asset_name" J.string
      , J.succeed "Lumens"
      ]
    )
    ( J.field "balance" J.string )

type alias Txn =
  { hash : String
  , ledger : Int
  , created_at : String
  , source_account : String
  , fee_paid : Int
  }

txnDecoder : J.Decoder Txn
txnDecoder =
  J.map5 Txn
    ( J.field "hash" J.string )
    ( J.field "ledger" J.int )
    ( J.field "created_at" J.string )
    ( J.field "source_account" J.string )
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
    ( J.field "type_" J.string )


viewThing : (String -> msg) -> Thing -> Html msg
viewThing nav t =
  case t of
    Address addr -> div [ class "box thing addr" ] [ viewAddr nav addr ]
    Transaction txn -> div [ class "box thing txn" ] [ viewTxn nav txn ]
    Operation op -> div [ class "box thing op" ] [ viewOp nav op ]
    Empty -> div [ class "box thing empty" ] []
    Errored err -> div [ class "box thing errored" ] [ text <| errorFormat err ]

viewAddr : (String -> msg) -> Addr -> Html msg
viewAddr nav addr =
  div []
    [ h1 [ class "title", title addr.id ] [ text <| "Address " ++ (wrap addr.id) ]
    , table []
        <| List.map (assetRow nav) addr.balances
    ]

assetRow : (String -> msg) -> Balance -> Html msg
assetRow nav balance =
  tr []
    [ td [] [ text balance.asset_name ]
    , td [] [ text balance.balance ]
    ]

viewTxn : (String -> msg) -> Txn -> Html msg
viewTxn nav txn =
  div []
    [ h1 [ class "title" ] [ text <| "Transaction " ++ (wrap txn.hash) ]
    , table []
      [ tr []
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
            ] [ text txn.source_account ]
          ]
        ]
      , tr []
        [ th [] [ text "fee_paid" ]
        , td [] [ text <| toString txn.fee_paid ]
        ]
      ]
    ]

viewOp : (String -> msg) -> Op -> Html msg
viewOp nav op =
  div []
    [ h1 [ class "title" ] [ text <| "Operation " ++ (wrap op.id) ]
    , table []
      [ tr []
        [ th [] [ text "source_account" ]
        , td [] [ text <| op.source_account ]
        ]
      , tr []
        [ th [] [ text "type" ]
        , td [] [ text op.type_ ]
        ]
      ]
    ]


wrap : String -> String
wrap hash = (String.left 3 hash) ++ "..." ++ (String.right 4 hash)

errorFormat : Http.Error -> String
errorFormat err =
  case err of
    Http.BadUrl u -> "bad url " ++ u
    Http.Timeout -> "timeout"
    Http.NetworkError -> "network error"
    Http.BadStatus resp ->
      resp.url ++ " returned " ++ (toString resp.status.code) ++ ": " ++ resp.body
    Http.BadPayload x y -> "bad payload (" ++ x ++ ")"
