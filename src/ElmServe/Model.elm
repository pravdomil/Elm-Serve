module ElmServe.Model exposing (..)

import Elm.Project
import ElmServe.Options
import JavaScript
import Parser


type alias Model =
    { options : Result (List Parser.DeadEnd) ElmServe.Options.Options
    , project : Result ProjectError Elm.Project.Project
    , compiler : Compiler
    , state : State
    }


type Compiler
    = CompilerReady
    | CompilerBusy


type State
    = NeedsRecompile
    | NoRecompile


type ProjectError
    = NotAsked
    | Loading
    | JavaScriptError JavaScript.Error
