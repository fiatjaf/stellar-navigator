port module Ports exposing (..)

port pushPage : (Bool, Int, String, String) -> Cmd msg

port surf : (Int -> msg) -> Sub msg

port navigate : (String -> msg) -> Sub msg
