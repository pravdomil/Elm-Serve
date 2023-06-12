module ElmServe.Msg exposing (..)

import ElmServe.Error
import ElmServe.Model
import Json.Decode
import Process


type Msg
    = NothingHappened
    | ModelReceived (Result ElmServe.Error.Error ElmServe.Model.Ready)
    | FileChanged String
    | CompileProcessReceived Process.Id
    | RequestReceived Request



--


type alias Request =
    { request : Json.Decode.Value
    , response : Json.Decode.Value
    }
