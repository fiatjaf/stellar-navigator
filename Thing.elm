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

thingDecoder : J.Decoder Thing
thingDecoder =
  J.oneOf
    [ addrDecoder |> J.map Address
    , txnDecoder |> J.map Transaction
    , opDecoder |> J.map Operation
    , J.null Empty
    ]

type alias Addr =
  { address : String
  , balances : List Balance
  }

addrDecoder : J.Decoder Addr
addrDecoder =
  J.map2 Addr
    ( J.field "address" J.string )
    ( J.field "balances" <| J.list balanceDecoder )

type alias Balance =
  { asset_type : String
  , balance : String
  }

balanceDecoder : J.Decoder Balance
balanceDecoder =
  J.map2 Balance
    ( J.field "asset_type" J.string )
    ( J.field "balance" J.string )

type alias Txn =
  { hash : String
  , ledger : Int
  , created : String
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


viewThing : Thing -> Html msg
viewThing t =
  case t of
    Address addr -> div [] [ text <| "address " ++ addr.address ]
    Transaction txn -> div [] [ text <| "transaction " ++ txn.hash ]
    Operation op -> div [] [ text <| "operation " ++ op.id ]
    Empty -> div [] [ text "nothing" ]
