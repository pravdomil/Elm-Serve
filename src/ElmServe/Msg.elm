module ElmServe.Msg exposing (..)

import ElmServe.Error
import ElmServe.Model
import Http.Server
import Process


type Msg
    = NothingHappened
    | ModelReceived (Result ElmServe.Error.Error ElmServe.Model.Ready)
    | FileChanged String
    | CompileProcessReceived Process.Id
    | RequestReceived Http.Server.Request
