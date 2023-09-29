module ElmServe.Model exposing (..)

import Elm.Project
import ElmServe.Options
import HttpServer
import JavaScript
import Parser


type alias Model =
    { options : Result (List Parser.DeadEnd) ElmServe.Options.Options
    , project : Result ProjectError Elm.Project.Project
    , compiler : Compiler
    , state : State
    , queue : List HttpServer.Request
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


type Error
    = CompileError JavaScript.Error
    | ServerError JavaScript.Error
    | WatchFilesError JavaScript.Error
    | ResponseError JavaScript.Error
    | QueueError JavaScript.Error
      --
    | ConsoleError JavaScript.Error
