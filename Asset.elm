module Asset exposing (..)

import Json.Decode as J


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
