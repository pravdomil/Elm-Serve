module ElmServe.Msg exposing (..)

import Elm.Project
import HttpServer
import JavaScript
import Json.Decode


type Msg
    = NothingHappened
      --
    | ProjectReceived (Result JavaScript.Error Elm.Project.Project)
      --
    | FileChanged (Result Json.Decode.Error String)
    | RequestReceived (Result Json.Decode.Error HttpServer.Request)
