module ElmServe.Model exposing (..)

import Elm.Project
import ElmServe.Options
import Process


type alias Model =
    Maybe ReadyModel



--


type alias ReadyModel =
    { options : ElmServe.Options.Options
    , project : Elm.Project.Project
    , compileProcess : Maybe Process.Id
    }
