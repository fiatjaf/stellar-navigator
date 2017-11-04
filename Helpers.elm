module Helpers exposing (..)

import Html exposing
  ( Html, Attribute, text
  , h1, h2, div, textarea, button, p, a
  , table, tbody, thead, tr, th, td
  , input, select, option, header, nav
  , span, section, nav, img, label
  )
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Http
import Hashbow
import Color
import Date
import Date.Format

wrap : String -> String
wrap str =
  if str == ""
  then ""
  else (String.left 3 str) ++ "..." ++ (String.right 4 str)
    |> String.toLower

hashcolor : String -> Attribute msg
hashcolor id =
  let 
    s = (String.toLower id)
    c = Hashbow.color 0.7 0.8 s
    rgba = Color.toRgb c
  in
    style
      [ ("background-color"
        , "rgba(" ++ (toString rgba.red) ++ ", " ++ (toString rgba.green) ++ ", " ++ (toString rgba.blue) ++ ", 0.7)"
        )
      ]

errorFormat : Http.Error -> String
errorFormat err =
  case err of
    Http.BadUrl u -> "bad url " ++ u
    Http.Timeout -> "timeout"
    Http.NetworkError -> "network error"
    Http.BadStatus resp ->
      resp.url ++ " returned " ++ (toString resp.status.code) ++ ": " ++ resp.body
    Http.BadPayload x y -> "bad payload (" ++ x ++ ")"


addrlink = link "addr"
txnlink = link "txn"
oplink = link "op"
ledglink = link "ledg"
efflink = link "eff"

link : String -> (String -> msg) -> String -> Html msg
link kind nav id =
  a
    [ onClick <| nav ("/" ++ kind ++ "/" ++ id)
    , title id
    , class <| "link " ++ kind
    , hashcolor id
    ] [ text <| wrap id ]


identifierKind : String -> String
identifierKind identifier =
  let
    len = String.length identifier
    int = String.toInt identifier |> Result.withDefault 0
  in
    if len == 56 then "addr"
    else if len == 64 then "txn"
    else if int == 0 then ""
    else if int < 100000000 then "ledg"
    else "op"
    

date : String -> String
date
  = Date.fromString
  >> Result.withDefault (Date.fromTime 0)
  >> Date.Format.format "%B %e, %Y, %I:%M %p"
