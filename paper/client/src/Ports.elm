port module Ports exposing (..)

import Json.Encode as E

port send : E.Value -> Cmd msg

port receive : (E.Value -> msg) -> Sub msg