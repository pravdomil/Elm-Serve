module ElmServe.Msg exposing (..)

import ElmServe.Error
import ElmServe.Model
import HttpServer
import Json.Decode
import Process


type Msg
    = NothingHappened
    | ModelReceived (Result ElmServe.Error.Error ElmServe.Model.Ready)
    | FileChanged (Result Json.Decode.Error String)
    | CompileProcessReceived Process.Id
    | RequestReceived (Result Json.Decode.Error HttpServer.Request)
