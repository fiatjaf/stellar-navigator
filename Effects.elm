module Effects exposing (..)

import Html exposing
  ( Html, Attribute, text
  , h1, h2, div, textarea, button, p, a
  , table, tbody, thead, tr, th, td
  , input, select, option, header
  , span, section, img, label
  )
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Json.Decode as J

import Asset exposing (..)
import Helpers exposing (..)

type EffData
  = AccountCreated { starting_balance : String }
  | AccountCredited Amt
  | AccountDebited Amt
  | AccountThresholdUpdated { low : Int, med : Int, high : Int }
  | AccountHomeDomainUpdated { home_domain : String }
  | AccountFlagsUpdated { auth_required : Bool, auth_revokable : Bool }
  | SignerCreated Signer
  | SignerRemoved Signer
  | SignerUpdated Signer
  | TrustlineCreated Line
  | TrustlineRemoved Line
  | TrustlineUpdated Line
  | TrustlineAuthorized TrustAuth
  | TrustlineDeauthorized TrustAuth
  | Trade Trd
  | NoEffData

effDataDecoder : String -> J.Decoder EffData
effDataDecoder op_type  =
  case op_type of
    "account_created" ->
      J.map ( (\a -> { starting_balance=a } ) >> AccountCreated )
        (  J.field "starting_balance" J.string  )
    "account_credited" ->
      J.map AccountCredited
        <| J.map Amt (  J.field "amount" J.string  )
    "account_debited" ->
      J.map AccountCredited
        <| J.map Amt (  J.field "amount" J.string  )
    "account_threshold_updated" ->
      J.map AccountThresholdUpdated
        <| J.map3
          ( curry3 (\(l,m,h ) -> { low=l, med=m, high=h }) )
          ( J.field "low_threshold" J.int  )
          ( J.field "med_threshold" J.int  )
          ( J.field "high_threshold" J.int  )
    "account_home_domain_updated" -> 
      J.map AccountHomeDomainUpdated
        <| J.map
          ( \hd -> { home_domain=hd } )
          ( J.field "home_domain" J.string )
    "account_flags_updated" -> 
      J.map AccountFlagsUpdated
        <| J.map2
          ( curry (\(req,rev) -> { auth_required=req, auth_revokable=rev }) )
          ( J.field "auth_required" J.bool )
          ( J.field "auth_revokable" J.bool )
    "signer_created" -> J.map SignerCreated signerDecoder
    "signer_removed" -> J.map SignerRemoved signerDecoder
    "signer_updated" -> J.map SignerUpdated signerDecoder
    "trustline_created" -> J.map TrustlineCreated lineDecoder
    "trustline_removed" -> J.map TrustlineRemoved lineDecoder
    "trustline_updated" -> J.map TrustlineUpdated lineDecoder
    "trustline_authorized" -> J.map TrustlineAuthorized trustAuthDecoder
    "trustline_deauthorized" -> J.map TrustlineDeauthorized trustAuthDecoder
    _ -> J.succeed NoEffData

type alias Amt = { amount : String }

type alias Signer = { weight : Int, public_key : String, key : String }

signerDecoder =
  J.map3 Signer
    ( J.field "weight" J.int )
    ( J.field "public_key" J.string )
    ( J.field "key" J.string )

type alias Line = { limit : String, asset : Asset }

lineDecoder =
  J.map2 Line
    ( J.field "limit" J.string )
    ( assetDecoder )

type alias TrustAuth = { trustor : String, asset : Asset }

trustAuthDecoder =
  J.map2 TrustAuth
    ( J.field "trustor" J.string )
    ( J.map3 Asset
      ( J.field "asset_type" J.string |> J.map ((==) "native") )
      ( J.map (Maybe.withDefault "")
        <| J.maybe ( J.field "asset_code" J.string )
      )
      ( ( J.field "account" J.string )
        |> J.andThen J.succeed -- will set this account as the asset.issuer
      )
    )

type alias Trd =
  { seller : String
  , offer_id : Int
  , sold_amount : String
  , sold_asset : Asset
  , bought_amount : String
  , bought_asset : Asset
  }

trdDecoder =
  J.map6 Trd
    ( J.field "seller" J.string )
    ( J.field "offer_id" J.int )
    ( J.field "sold_amount" J.string )
    ( J.map3 Asset
      ( J.field "sold_asset_type" J.string |> J.map ((==) "native") )
      ( J.map (Maybe.withDefault "")
        <| J.maybe ( J.field "sold_asset_code" J.string )
      )
      ( J.map (Maybe.withDefault "")
        <| J.maybe ( J.field "sold_asset_issuer" J.string )
      )
    )
    ( J.field "bought_amount" J.string )
    ( J.map3 Asset
      ( J.field "bought_asset_type" J.string |> J.map ((==) "native") )
      ( J.map (Maybe.withDefault "")
        <| J.maybe ( J.field "bought_asset_code" J.string )
      )
      ( J.map (Maybe.withDefault "")
        <| J.maybe ( J.field "bought_asset_issuer" J.string )
      )
    )
