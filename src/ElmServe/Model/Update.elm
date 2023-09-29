module ElmServe.Model.Update exposing (..)

import Console
import Dict
import Elm.Compiler
import Elm.Project
import ElmServe.Model
import ElmServe.Model.Utils
import ElmServe.Msg
import ElmServe.Options
import ElmServe.Path
import FileStatus
import FileSystem
import FileWatch
import HttpServer
import JavaScript
import Json.Decode
import Json.Encode
import Parser
import Platform.Extra
import Process.Extra
import Regex
import Task
import Task.Extra
import Url


init : Json.Decode.Value -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
init flags =
    ( ElmServe.Model.Model
        (ElmServe.Options.fromFlags flags)
        (Err ElmServe.Model.NotAsked)
        ElmServe.Model.CompilerReady
        ElmServe.Model.NeedsRecompile
        []
    , Cmd.none
    )
        |> Platform.Extra.andThen loadProject



--


update : ElmServe.Msg.Msg -> ElmServe.Model.Model -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
update msg =
    (case msg of
        ElmServe.Msg.NothingHappened ->
            Platform.Extra.noOperation

        ElmServe.Msg.ProjectReceived a ->
            projectReceived a

        ElmServe.Msg.ProjectCompiled a ->
            projectCompiled a

        ElmServe.Msg.FileChanged _ ->
            \x -> ( { x | state = ElmServe.Model.NeedsRecompile }, Cmd.none )

        ElmServe.Msg.RequestReceived a ->
            requestReceived a
    )
        >> Platform.Extra.andThen maybeRecompile


subscriptions : ElmServe.Model.Model -> Sub ElmServe.Msg.Msg
subscriptions _ =
    Sub.batch
        [ FileWatch.subscription ElmServe.Msg.FileChanged
        , HttpServer.subscription ElmServe.Msg.RequestReceived
        ]



--


loadProject : ElmServe.Model.Model -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
loadProject model =
    ( { model | project = Err ElmServe.Model.Loading }
    , Task.attempt
        ElmServe.Msg.ProjectReceived
        (ElmServe.Model.Utils.readProject "elm.json")
    )


projectReceived : Result JavaScript.Error Elm.Project.Project -> ElmServe.Model.Model -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
projectReceived a model =
    case a of
        Ok b ->
            case model.options of
                Ok c ->
                    ( { model | project = Ok b, compiler = ElmServe.Model.CompilerBusy, state = ElmServe.Model.NoRecompile }
                    , Task.attempt
                        ElmServe.Msg.ProjectCompiled
                        (Task.mapError ElmServe.Model.ConsoleError (Console.log "Elm Serve")
                            |> Task.andThen (\_ -> Task.mapError ElmServe.Model.CompileError (makeOutputFile c))
                            |> Task.andThen (\_ -> startServer c)
                            |> Task.andThen (\_ -> startWatching b)
                        )
                    )

                Err c ->
                    ( { model | project = Ok b }
                    , Task.attempt
                        (\_ -> ElmServe.Msg.NothingHappened)
                        (consoleErrorAndExit 1
                            (case Maybe.map .problem (List.head c) of
                                Just (Parser.Problem d) ->
                                    d

                                _ ->
                                    ElmServe.Model.Utils.usage
                            )
                        )
                    )

        Err b ->
            ( { model | project = Err (ElmServe.Model.JavaScriptError b) }
            , Task.attempt
                (\_ -> ElmServe.Msg.NothingHappened)
                (case b of
                    JavaScript.Exception _ (JavaScript.ErrorCode "ENOENT") _ ->
                        consoleErrorAndExit 1 "Cannot find elm.json."

                    _ ->
                        consoleErrorAndExit 1 ("Cannot read elm.json. " ++ JavaScript.errorToString b)
                )
            )



--


maybeRecompile : ElmServe.Model.Model -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
maybeRecompile model =
    case model.state of
        ElmServe.Model.NeedsRecompile ->
            case model.compiler of
                ElmServe.Model.CompilerReady ->
                    case model.options of
                        Ok b ->
                            ( { model | compiler = ElmServe.Model.CompilerBusy, state = ElmServe.Model.NoRecompile }
                            , Task.attempt
                                ElmServe.Msg.ProjectCompiled
                                (Task.mapError ElmServe.Model.CompileError (makeOutputFile b))
                            )

                        Err _ ->
                            Platform.Extra.noOperation model

                ElmServe.Model.CompilerBusy ->
                    Platform.Extra.noOperation model

        ElmServe.Model.NoRecompile ->
            Platform.Extra.noOperation model


projectCompiled : Result ElmServe.Model.Error () -> ElmServe.Model.Model -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
projectCompiled a model =
    case a of
        Ok _ ->
            ( { model | compiler = ElmServe.Model.CompilerReady, queue = [] }
            , Task.attempt
                (\_ -> ElmServe.Msg.NothingHappened)
                (Task.sequence (List.map (\x -> send 200 (Dict.singleton "Access-Control-Allow-Origin" "*") "" x) model.queue))
            )

        Err b ->
            ( model
            , Task.attempt
                (\_ -> ElmServe.Msg.NothingHappened)
                (consoleErrorAndExit 1 (ElmServe.Model.Utils.errorToString b))
            )



--


requestReceived : Result Json.Decode.Error HttpServer.Request -> ElmServe.Model.Model -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
requestReceived a model =
    case a of
        Ok b ->
            case ElmServe.Path.fromRequest b of
                Ok path ->
                    case model.options of
                        Ok options ->
                            case path of
                                "/elm-serve-client-lib.js" ->
                                    ( { model | queue = b :: model.queue }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model
                                    , Task.attempt
                                        (\_ -> ElmServe.Msg.NothingHappened)
                                        (FileStatus.get (FileSystem.stringToPath (options.root ++ "/" ++ path))
                                            |> Task.andThen
                                                (\x ->
                                                    case x of
                                                        FileStatus.File ->
                                                            sendFile options path b

                                                        FileStatus.Directory ->
                                                            redirect (path ++ "/") b

                                                        FileStatus.NotFound ->
                                                            if options.no404 then
                                                                sendFile options "index.html" b

                                                            else
                                                                send 404 Dict.empty "Not found." b
                                                )
                                        )
                                    )

                        Err _ ->
                            ( model
                            , Task.attempt
                                (\_ -> ElmServe.Msg.NothingHappened)
                                (send 500 Dict.empty "Server is not ready." b)
                            )

                Err c ->
                    ( model
                    , Task.attempt
                        (\_ -> ElmServe.Msg.NothingHappened)
                        (case c of
                            ElmServe.Path.CannotParseUrl ->
                                send 400 Dict.empty "Bad request - cannot parse url." b

                            ElmServe.Path.ParentFolderPath ->
                                send 403 Dict.empty "Forbidden - cannot go to parent folder." b
                        )
                    )

        Err _ ->
            Platform.Extra.noOperation model



--


makeOutputFile : ElmServe.Options.Options -> Task.Task JavaScript.Error ()
makeOutputFile options =
    let
        outputPath : FileSystem.Path
        outputPath =
            FileSystem.stringToPath options.elm.output

        recoverFromCompileError : JavaScript.Error -> Task.Task JavaScript.Error String
        recoverFromCompileError b =
            case b of
                JavaScript.Exception _ (JavaScript.ErrorCode "ENONZERO") (JavaScript.ErrorMessage c) ->
                    Console.log c
                        |> Task.Extra.andAlwaysThen (\_ -> Task.succeed (ElmServe.Model.Utils.reportCompileErrorJs c))

                _ ->
                    Task.fail b
    in
    Console.log "Compiling..."
        |> Task.andThen (\_ -> Elm.Compiler.compile options.elm)
        |> Task.andThen (\_ -> Task.onError (\_ -> Task.succeed "") (ElmServe.Model.Utils.elmFfi options.elm.output))
        |> Task.andThen (\_ -> FileSystem.read outputPath)
        |> Task.andThen (\x -> ElmServe.Model.Utils.elmHot x)
        |> Task.onError recoverFromCompileError
        |> Task.map (\x -> ElmServe.Model.Utils.jsLibrary ++ x)
        |> Task.andThen (FileSystem.write outputPath)



--


startServer : ElmServe.Options.Options -> Task.Task ElmServe.Model.Error ()
startServer options =
    let
        url : String
        url =
            HttpServer.url options.server

        open : Task.Task x ()
        open =
            if options.open then
                ElmServe.Model.Utils.open url
                    |> Task.onError (\_ -> Task.succeed ())

            else
                Task.succeed ()
    in
    HttpServer.start options.server
        |> Task.mapError ElmServe.Model.ServerError
        |> Task.andThen (\_ -> Task.mapError ElmServe.Model.ConsoleError (Console.log ("Server is running at:\n" ++ url)))
        |> Task.andThen (\_ -> open)


startWatching : Elm.Project.Project -> Task.Task ElmServe.Model.Error ()
startWatching a =
    let
        paths : List String
        paths =
            case a of
                Elm.Project.Application b ->
                    "elm.json" :: b.dirs

                Elm.Project.Package _ ->
                    [ "elm.json", "src" ]
    in
    Task.sequence (List.map (\x -> FileWatch.watch (FileSystem.stringToPath x)) paths)
        |> Task.map (\_ -> ())
        |> Task.mapError ElmServe.Model.WatchFilesError



--


send : Int -> Dict.Dict String String -> String -> HttpServer.Request -> Task.Task JavaScript.Error ()
send status headers data { response } =
    JavaScript.run "a.res.writeHead(a.status, a.headers).end(a.data)"
        (Json.Encode.object
            [ ( "status", Json.Encode.int status )
            , ( "headers", Json.Encode.dict identity Json.Encode.string headers )
            , ( "data", Json.Encode.string data )
            , ( "res", response )
            ]
        )
        (Json.Decode.succeed ())


sendFile : ElmServe.Options.Options -> String -> HttpServer.Request -> Task.Task JavaScript.Error ()
sendFile options path { request, response } =
    JavaScript.run "require('send')(a.req, a.path, { root: a.root }).pipe(a.res)"
        (Json.Encode.object
            [ ( "root", Json.Encode.string options.root )
            , ( "path", Json.Encode.string path )
            , ( "req", request )
            , ( "res", response )
            ]
        )
        (Json.Decode.succeed ())


redirect : String -> HttpServer.Request -> Task.Task JavaScript.Error ()
redirect path a =
    send 301 (Dict.fromList [ ( "Location", path ) ]) ("Moved permanently to " ++ path ++ ".") a


consoleErrorAndExit : Int -> String -> Task.Task x ()
consoleErrorAndExit code a =
    taskTwo
        (Console.logError a)
        (Process.Extra.hardExit code)
        |> Task.map (\_ -> ())


taskTwo : Task.Task x a -> Task.Task x b -> Task.Task y ( Result x a, Result x b )
taskTwo a b =
    Task.Extra.andAlwaysThen
        (\x ->
            Task.Extra.andAlwaysThen
                (\x2 ->
                    Task.succeed ( x, x2 )
                )
                b
        )
        a
