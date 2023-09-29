module ElmServe.Model.Update exposing (..)

import Console
import Dict
import Elm.Compiler
import Elm.Project
import ElmServe.Model
import ElmServe.Model.Utils
import ElmServe.Msg
import ElmServe.Options
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
                            |> Task.andThen (\_ -> makeOutputFile c)
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
                                (Task.mapError ElmServe.Model.ConsoleError (Console.log "Recompiling...")
                                    |> Task.andThen (\_ -> makeOutputFile b)
                                    |> Task.andThen (\_ -> resolveQueue)
                                )
                            )

                        Err _ ->
                            Platform.Extra.noOperation model

                ElmServe.Model.CompilerBusy ->
                    Platform.Extra.noOperation model

        ElmServe.Model.NoRecompile ->
            Platform.Extra.noOperation model


projectCompiled : Result ElmServe.Model.Error () -> ElmServe.Model.Model -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
projectCompiled a model =
    ( { model | compiler = ElmServe.Model.CompilerReady }
    , case a of
        Ok _ ->
            Cmd.none

        Err b ->
            Task.attempt
                (\_ -> ElmServe.Msg.NothingHappened)
                (consoleErrorAndExit 1 (ElmServe.Model.Utils.errorToString b))
    )



--


requestReceived : Result Json.Decode.Error HttpServer.Request -> ElmServe.Model.Model -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
requestReceived a model =
    case a of
        Ok b ->
            ( model
            , Task.attempt
                (\_ -> ElmServe.Msg.NothingHappened)
                (case model.options of
                    Ok c ->
                        sendResponse c b

                    Err _ ->
                        send 500 Dict.empty "Server is not ready." b
                )
            )

        Err _ ->
            Platform.Extra.noOperation model



--


makeOutputFile : ElmServe.Options.Options -> Task.Task ElmServe.Model.Error ()
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
    Elm.Compiler.compile options.elm
        |> Task.andThen (\_ -> Task.onError (\_ -> Task.succeed "") (ElmServe.Model.Utils.elmFfi options.elm.output))
        |> Task.andThen (\_ -> FileSystem.read outputPath)
        |> Task.andThen (\x -> ElmServe.Model.Utils.elmHot x)
        |> Task.onError recoverFromCompileError
        |> Task.map (\x -> ElmServe.Model.Utils.jsLibrary ++ x)
        |> Task.andThen (FileSystem.write outputPath)
        |> Task.mapError ElmServe.Model.CompileError


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


type RespondError
    = CannotParseUrl
    | ParentFolderPath
    | NotFound
    | InternalError JavaScript.Error


sendResponse : ElmServe.Options.Options -> HttpServer.Request -> Task.Task JavaScript.Error ()
sendResponse options a =
    let
        errorResponse : RespondError -> Task.Task JavaScript.Error ()
        errorResponse b =
            case b of
                CannotParseUrl ->
                    send 400 Dict.empty "Bad request - cannot parse url." a

                ParentFolderPath ->
                    send 403 Dict.empty "Forbidden - cannot go to parent folder." a

                NotFound ->
                    send 404 Dict.empty "Not found." a

                InternalError c ->
                    send 500 Dict.empty "Server error." a
                        |> Task.andThen (\_ -> Task.fail c)
    in
    requestPath a
        |> Task.Extra.fromResult
        |> Task.andThen (resolvePath options a)
        |> Task.onError errorResponse


requestPath : HttpServer.Request -> Result RespondError String
requestPath a =
    let
        parentFolderRegex : Regex.Regex
        parentFolderRegex =
            Regex.fromString "(^|/)\\.\\.(/|$)"
                |> Maybe.withDefault Regex.never
    in
    Json.Decode.decodeValue (Json.Decode.field "url" Json.Decode.string) a.request
        |> Result.map (\x -> "http://localhost" ++ x)
        |> Result.toMaybe
        |> Maybe.andThen Url.fromString
        |> Maybe.map .path
        |> Maybe.andThen Url.percentDecode
        |> Result.fromMaybe CannotParseUrl
        |> Result.andThen
            (\x ->
                if Regex.contains parentFolderRegex x then
                    Err ParentFolderPath

                else
                    Ok x
            )
        |> Result.map
            (\x ->
                if String.endsWith "/" x then
                    x ++ "index.html"

                else
                    x
            )


resolvePath : ElmServe.Options.Options -> HttpServer.Request -> String -> Task.Task RespondError ()
resolvePath options req a =
    let
        redirect : String -> Task.Task RespondError ()
        redirect b =
            send 301 (Dict.fromList [ ( "Location", b ) ]) ("Moved permanently to " ++ b ++ ".") req
                |> Task.mapError InternalError
    in
    if a == "/elm-serve-client-lib.js" then
        addRequestToQueue req

    else
        FileStatus.get (FileSystem.stringToPath (options.root ++ "/" ++ a))
            |> Task.mapError InternalError
            |> Task.andThen
                (\x ->
                    case x of
                        FileStatus.File ->
                            sendFile options a req
                                |> Task.mapError InternalError

                        FileStatus.Directory ->
                            redirect (a ++ "/")

                        FileStatus.NotFound ->
                            if options.no404 then
                                sendFile options "index.html" req
                                    |> Task.mapError InternalError

                            else
                                Task.fail NotFound
                )



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


addRequestToQueue : HttpServer.Request -> Task.Task RespondError ()
addRequestToQueue a =
    JavaScript.run "(() => { if (!global.queue) global.queue = []; queue.push(a); })()"
        (Json.Encode.object
            [ ( "req", a.request )
            , ( "res", a.response )
            ]
        )
        (Json.Decode.succeed ())
        |> Task.mapError InternalError


resolveQueue : Task.Task ElmServe.Model.Error ()
resolveQueue =
    JavaScript.run "(() => { if (!global.queue) global.queue = []; queue.forEach(a => { a.res.setHeader('Access-Control-Allow-Origin', '*'); a.res.end(); }); queue = []; })()"
        Json.Encode.null
        (Json.Decode.succeed ())
        |> Task.mapError ElmServe.Model.QueueError


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



--


consoleErrorAndExit : Int -> String -> Task.Task x ()
consoleErrorAndExit code a =
    taskTwo
        (Console.logError a)
        (Process.Extra.hardExit code)
        |> Task.map (\_ -> ())



--


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
