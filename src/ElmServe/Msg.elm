module ElmServe.Msg exposing (..)

import Elm.Project
import HttpServer
import JavaScript
import Json.Decode
import Process


type Msg
    = NothingHappened
      --
    | ProjectReceived (Result JavaScript.Error Elm.Project.Project)
      --
    | FileChanged (Result Json.Decode.Error String)
    | CompileProcessReceived Process.Id
    | RequestReceived (Result Json.Decode.Error HttpServer.Request)
