module ElmServe.Model exposing (..)

import Elm.Project
import ElmServe.Error
import ElmServe.Options
import Process


type alias Model =
    Result Error ReadyModel



--


type alias ReadyModel =
    { options : ElmServe.Options.Options
    , project : Elm.Project.Project
    , compileProcess : Maybe Process.Id
    }



--


type Error
    = Loading
    | Error ElmServe.Error.Error
