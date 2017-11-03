module Operations exposing (..)

import Json.Decode as J
import Asset exposing (..)

type OpData
  = CreateAccount Create
  | Payment Pay
  | PathPayment PathPay
  | ManageOfferOrPassive Offer
  | SetOptions SetOpt
  | ChangeTrust Trust
  | AllowTrust Allow
  | AccountMerge Merge
  | Inflation Infl
  | ManageData ManData
  | None

opDataDecoder : J.Decoder OpData
opDataDecoder =
  J.oneOf
    [ createDecoder
    , payDecoder
    , pathPayDecoder
    , offerDecoder
    , setOptDecoder
    , trustDecoder
    , allowDecoder
    , mergeDecoder
    , inflDecoder
    , manDataDecoder
    , J.null None
    ]

type alias Create =
  { funder : String
  , starting_balance : String
  , account : String
  }

createDecoder : J.Decoder OpData
createDecoder =
  J.map CreateAccount <| J.map3 Create
    ( J.field "funder" J.string )
    ( J.field "starting_balance" J.string )
    ( J.field "account" J.string )


type alias Pay =
  { asset : Asset
  , from : String
  , to : String
  , amount : String
  }

payDecoder : J.Decoder OpData
payDecoder =
  J.map Payment <| J.map4 Pay
    ( assetDecoder )
    ( J.field "from" J.string )
    ( J.field "to" J.string )
    ( J.field "amount" J.string )


type alias PathPay =
  { asset : Asset
  , from : String
  , to : String
  , amount : String
  , path : List Asset
  }

pathPayDecoder : J.Decoder OpData
pathPayDecoder =
  J.map PathPayment <| J.map5 PathPay
    ( assetDecoder )
    ( J.field "from" J.string )
    ( J.field "to" J.string )
    ( J.field "amount" J.string )
    ( J.field "path" <| J.list assetDecoder )


type alias Offer =
  { offer_id : Int
  , amount : String
  , price : String
  , buying : Asset
  , selling : Asset
  }

offerDecoder : J.Decoder OpData
offerDecoder =
  J.map ManageOfferOrPassive <| J.map5 Offer
    ( J.field "offer_id" J.int )
    ( J.field "amount" J.string )
    ( J.field "price" J.string )
    ( J.field "buying"
      <| J.map3 Asset
        ( J.field "buying_asset_type" J.string |> J.map ((==) "native") )
        ( J.field "buying_asset_code" J.string )
        ( J.field "buying_asset_issuer" J.string )
    )
    ( J.field "selling"
      <| J.map3 Asset
        ( J.field "selling_asset_type" J.string |> J.map ((==) "native") )
        ( J.field "selling_asset_code" J.string )
        ( J.field "selling_asset_issuer" J.string )
    )


type alias SetOpt =
  { inflation_dest : String
  , home_domain : String
  , signer_key : String
  , signer_weight : Int
  , master_key_weight : Int
  , low_threshold : Int
  , med_threshold : Int
  , high_threshold : Int
  }

setOptDecoder : J.Decoder OpData
setOptDecoder =
  J.map SetOptions <| J.map8 SetOpt
    ( J.field "inflation_dest" J.string )
    ( J.field "home_domain" J.string )
    ( J.field "signer_key" J.string )
    ( J.field "signer_weight" J.int )
    ( J.field "master_key_weight" J.int )
    ( J.field "low_threshold" J.int )
    ( J.field "med_threshold" J.int )
    ( J.field "high_threshold" J.int )


type alias Trust =
  { asset : Asset
  , limit : String
  , trustee : String
  , trustor : String
  }

trustDecoder : J.Decoder OpData
trustDecoder =
  J.map ChangeTrust <| J.map4 Trust
    ( assetDecoder )
    ( J.field "limit" J.string )
    ( J.field "trustee" J.string )
    ( J.field "trustor" J.string )


type alias Allow =
  { asset : Asset
  , limit : String
  , trustee : String
  , trustor : String
  , authorize : Bool
  }

allowDecoder : J.Decoder OpData
allowDecoder =
  J.map AllowTrust <| J.map5 Allow
    ( assetDecoder )
    ( J.field "limit" J.string )
    ( J.field "trustee" J.string )
    ( J.field "trustor" J.string )
    ( J.field "authorize" J.bool )


type alias Merge =
  { account : String
  , into : String
  }

mergeDecoder : J.Decoder OpData
mergeDecoder =
  J.map AccountMerge <| J.map2 Merge
    ( J.field "account" J.string )
    ( J.field "into" J.string )


type alias Infl = {}

inflDecoder : J.Decoder OpData
inflDecoder =
  J.map Inflation <| J.null Infl


type alias ManData =
  { name : String
  , value : String
  }

manDataDecoder : J.Decoder OpData
manDataDecoder =
  J.map ManageData <| J.map2 ManData
    ( J.field "name" J.string )
    ( J.field "value" J.string )
