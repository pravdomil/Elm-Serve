module ElmServe.Model exposing (..)

import Elm.Project
import ElmServe.Options
import Parser
import Process


type alias Model =
    { options : Result (List Parser.DeadEnd) ElmServe.Options.Options
    , project : Result ProjectError Elm.Project.Project
    , compileProcess : Maybe Process.Id
    }



--


type ProjectError
    = NotAsked
    | Loading
    | ParseError (List Parser.DeadEnd)
