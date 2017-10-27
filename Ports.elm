port module Ports exposing (..)


port pushPage : (Int, String, String) -> Cmd msg

port surf : (Int -> msg) -> Sub msg

port navigate : (String -> msg) -> Sub msg
