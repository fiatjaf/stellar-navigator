module Asset exposing (..)

import Html exposing
  ( Html, Attribute, text
  , h1, h2, div, textarea, button, p, a
  , table, tbody, thead, tr, th, td
  , input, select, option, header, nav
  , span, section, nav, img, label
  )
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Json.Decode as J

import Helpers exposing (..)


type alias Asset =
  { native : Bool
  , code : String
  , issuer : String
  }

assetDecoder : J.Decoder Asset
assetDecoder =
  J.map3 Asset
    ( J.field "asset_type" J.string |> J.map ((==) "native") )
    ( J.map (Maybe.withDefault "Lumens")
      <| J.maybe ( J.field "asset_code" J.string )
    )
    ( J.map (Maybe.withDefault "")
      <| J.maybe ( J.field "asset_issuer" J.string )
    )

viewAsset : (String -> msg) -> Asset -> Html msg
viewAsset nav asset =
  if asset.native then span [ class "asset-native" ] [ text "lumens" ]
  else span [ class "asset-issued" ]
    [ span [ class "code" ]
      [ text asset.code
      ]
    , addrlink nav asset.issuer
    ]
