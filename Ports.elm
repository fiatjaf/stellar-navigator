port module Ports exposing (..)

port pushPage : (Bool, Int, String, String) -> Cmd msg

port sse : String -> Cmd msg

port surf : (Int -> msg) -> Sub msg

port navigate : (String -> msg) -> Sub msg

port newop : (String -> msg) -> Sub msg

port newledg : (String -> msg) -> Sub msg
