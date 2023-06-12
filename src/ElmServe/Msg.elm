module ElmServe.Msg exposing (..)

import ElmServe.Error
import ElmServe.Model
import Json.Decode
import Process


type Msg
    = GotReadyModel (Result ElmServe.Error.Error ElmServe.Model.ReadyModel)
    | GotFileChange { path : String }
    | GotCompileProcess Process.Id
    | GotRequest Request
    | TaskDone (Result ElmServe.Error.Error ())



--


type alias Request =
    { request : Json.Decode.Value
    , response : Json.Decode.Value
    }
