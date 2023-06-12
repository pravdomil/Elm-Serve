module ElmServe.Model exposing (..)

import Elm.Project
import ElmServe.Error
import ElmServe.Options
import Process


type alias Model =
    Result Error Ready



--


type alias Ready =
    { options : ElmServe.Options.Options
    , project : Elm.Project.Project
    , compileProcess : Maybe Process.Id
    }



--


type Error
    = NotAsked
    | Loading
    | Error ElmServe.Error.Error
