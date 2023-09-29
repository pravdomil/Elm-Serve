module ElmServe.Msg exposing (..)

import Elm.Project
import ElmServe.Model
import HttpServer
import JavaScript
import Json.Decode


type Msg
    = NothingHappened
      --
    | ProjectReceived (Result JavaScript.Error Elm.Project.Project)
    | ProjectCompiled (Result ElmServe.Model.Error ())
      --
    | FileChanged (Result Json.Decode.Error String)
    | RequestReceived (Result Json.Decode.Error HttpServer.Request)
