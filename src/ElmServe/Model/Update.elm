module ElmServe.Model.Update exposing (..)

import Console
import Dict
import Elm.Compiler
import Elm.Project
import ElmServe.Error
import ElmServe.Model
import ElmServe.Msg
import ElmServe.Options
import ElmServe.Utils.Utils
import FileSystem
import FileWatch
import HttpServer
import JavaScript
import Json.Decode
import Json.Encode
import Platform.Extra
import Process
import Process.Extra
import Regex
import Task
import Task.Extra
import Url


init : Json.Decode.Value -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
init flags =
    ( Err ElmServe.Model.NotAsked
    , Cmd.none
    )
        |> Platform.Extra.andThen (initModel flags)


initModel : Json.Decode.Value -> ElmServe.Model.Model -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
initModel flags _ =
    let
        options : Task.Task ElmServe.Error.Error ElmServe.Options.Options
        options =
            Json.Decode.decodeValue
                (Json.Decode.at [ "global", "process", "argv" ] (Json.Decode.list Json.Decode.string))
                flags
                |> Result.withDefault []
                |> (\x ->
                        case ElmServe.Options.parse (List.drop 2 x) of
                            Ok x2 ->
                                Task.succeed x2

                            Err x2 ->
                                Task.fail (ElmServe.Error.CannotParseOptions x2)
                   )

        cmd : Cmd ElmServe.Msg.Msg
        cmd =
            Task.map3
                ElmServe.Model.Ready
                options
                (ElmServe.Utils.Utils.readProject "elm.json" |> Task.mapError ElmServe.Error.CannotReadProject)
                (Task.succeed Nothing)
                |> Task.attempt ElmServe.Msg.ModelReceived
    in
    ( Err ElmServe.Model.Loading
    , cmd
    )



--


update : ElmServe.Msg.Msg -> ElmServe.Model.Model -> ( ElmServe.Model.Model, Cmd ElmServe.Msg.Msg )
update msg =
    case msg of
        ElmServe.Msg.NothingHappened ->
            Platform.Extra.noOperation

        ElmServe.Msg.ModelReceived a ->
            \_ ->
                case a of
                    Ok b ->
                        let
                            task : Task.Task ElmServe.Error.Error ()
                            task =
                                (Console.log "Elm Serve\n\n" |> Task.mapError ElmServe.Error.ConsoleError)
                                    |> Task.andThen (\_ -> makeOutputFile b.options)
                                    |> Task.andThen (\_ -> startServer b.options)
                                    |> Task.andThen (\_ -> startWatching b.project)
                                    |> Task.onError (\x -> exitWithMessageAndCode (ElmServe.Error.toString x) 1)
                        in
                        ( Ok b
                        , task
                            |> Task.attempt (\_ -> ElmServe.Msg.NothingHappened)
                        )

                    Err b ->
                        ( Err (ElmServe.Model.Error b)
                        , exitWithMessageAndCode (ElmServe.Error.toString b) 1
                            |> Task.attempt (\_ -> ElmServe.Msg.NothingHappened)
                        )

        ElmServe.Msg.FileChanged _ ->
            \model ->
                let
                    killCompileProcess : ElmServe.Model.Ready -> Task.Task x ()
                    killCompileProcess b =
                        case b.compileProcess of
                            Just x ->
                                Process.kill x

                            Nothing ->
                                Task.succeed ()

                    task : Task.Task ElmServe.Error.Error ()
                    task =
                        case model of
                            Ok b ->
                                killCompileProcess b
                                    |> Task.andThen (\_ -> Process.sleep 0.5)
                                    |> Task.andThen (\_ -> Console.log "Recompiling..." |> Task.mapError ElmServe.Error.ConsoleError)
                                    |> Task.andThen (\_ -> makeOutputFile b.options)
                                    |> Task.andThen (\_ -> resolveQueue)
                                    |> Task.onError (\x -> exitWithMessageAndCode (ElmServe.Error.toString x) 1)

                            Err _ ->
                                Task.succeed ()
                in
                ( model
                , Process.spawn task
                    |> Task.perform ElmServe.Msg.CompileProcessReceived
                )

        ElmServe.Msg.CompileProcessReceived a ->
            \model -> ( model |> Result.map (\v -> { v | compileProcess = Just a }), Cmd.none )

        ElmServe.Msg.RequestReceived a ->
            \model ->
                case model of
                    Ok b ->
                        ( model
                        , sendResponse b.options a
                            |> Task.attempt (\_ -> ElmServe.Msg.NothingHappened)
                        )

                    Err _ ->
                        ( model
                        , send 500 Dict.empty "Server is not ready." a
                            |> Task.attempt (\_ -> ElmServe.Msg.NothingHappened)
                        )


subscriptions : ElmServe.Model.Model -> Sub ElmServe.Msg.Msg
subscriptions _ =
    Sub.batch
        [ FileWatch.subscription ElmServe.Msg.FileChanged
        , HttpServer.requestSubscription ElmServe.Msg.RequestReceived
        ]



--


makeOutputFile : ElmServe.Options.Options -> Task.Task ElmServe.Error.Error ()
makeOutputFile options =
    let
        outputPath : FileSystem.Path
        outputPath =
            FileSystem.stringToPath options.elm.output

        recoverFromCompileError : JavaScript.Error -> Task.Task JavaScript.Error String
        recoverFromCompileError b =
            case b of
                JavaScript.Exception _ (JavaScript.ErrorCode "ENONZERO") (JavaScript.ErrorMessage c) ->
                    Task.succeed (ElmServe.Utils.Utils.reportCompileErrorJs c)

                _ ->
                    Task.fail b
    in
    Elm.Compiler.compile options.elm
        |> Task.andThen (\_ -> FileSystem.read outputPath)
        |> Task.andThen (\x -> ElmServe.Utils.Utils.patchElm x)
        |> Task.onError recoverFromCompileError
        |> Task.map (\x -> ElmServe.Utils.Utils.jsLibrary ++ x)
        |> Task.andThen (FileSystem.write outputPath)
        |> Task.mapError ElmServe.Error.CannotCompileElm


startServer : ElmServe.Options.Options -> Task.Task ElmServe.Error.Error ()
startServer options =
    let
        url : String
        url =
            HttpServer.url options.server

        open : Task.Task x ()
        open =
            if options.open then
                ElmServe.Utils.Utils.open url
                    |> Task.onError (\_ -> Task.succeed ())

            else
                Task.succeed ()
    in
    HttpServer.start options.server
        |> Task.mapError ElmServe.Error.CannotStartServer
        |> Task.andThen (\_ -> Console.log ("Server is running at:\n" ++ url ++ "\n") |> Task.mapError ElmServe.Error.ConsoleError)
        |> Task.andThen (\_ -> open)


startWatching : Elm.Project.Project -> Task.Task ElmServe.Error.Error ()
startWatching a =
    let
        dirs : List String
        dirs =
            case a of
                Elm.Project.Application b ->
                    b.dirs

                Elm.Project.Package _ ->
                    [ "src" ]
    in
    Task.sequence (List.map (\x -> FileWatch.watch (FileSystem.stringToPath x)) dirs)
        |> Task.map (\_ -> ())
        |> Task.mapError ElmServe.Error.CannotWatchFiles



--


type RespondError
    = CannotParseUrl
    | ParentFolderPath
    | NotFound_
    | InternalError_ JavaScript.Error


sendResponse : ElmServe.Options.Options -> HttpServer.Request -> Task.Task ElmServe.Error.Error ()
sendResponse opt a =
    let
        resolvePath : String -> Task.Task RespondError ()
        resolvePath b =
            if b == "/elm-serve-client-lib.js" then
                addRequestToQueue a

            else
                fileStatus (opt.root ++ "/" ++ b)
                    |> Task.mapError InternalError_
                    |> Task.andThen
                        (\v ->
                            case v of
                                File ->
                                    sendFile opt b a
                                        |> Task.mapError InternalError_

                                Directory ->
                                    redirect (b ++ "/")

                                NotFound ->
                                    if opt.no404 then
                                        sendFile opt "index.html" a
                                            |> Task.mapError InternalError_

                                    else
                                        Task.fail NotFound_
                        )

        redirect : String -> Task.Task RespondError ()
        redirect b =
            send 301 (Dict.fromList [ ( "Location", b ) ]) ("Moved permanently to " ++ b ++ ".") a
                |> Task.mapError InternalError_

        errorResponse : RespondError -> Task.Task JavaScript.Error ()
        errorResponse b =
            case b of
                CannotParseUrl ->
                    send 400 Dict.empty "Bad request - cannot parse url." a

                ParentFolderPath ->
                    send 403 Dict.empty "Forbidden - cannot go to parent folder." a

                NotFound_ ->
                    send 404 Dict.empty "Not found." a

                InternalError_ c ->
                    send 500 Dict.empty "Server error." a
                        |> Task.andThen (\_ -> Task.fail c)
    in
    requestPath a
        |> Task.andThen resolvePath
        |> Task.onError errorResponse
        |> Task.mapError ElmServe.Error.InternalError


requestPath : HttpServer.Request -> Task.Task RespondError String
requestPath { request } =
    let
        parentFolderRegex : Regex.Regex
        parentFolderRegex =
            Regex.fromString "(^|/)\\.\\.(/|$)"
                |> Maybe.withDefault Regex.never
    in
    Json.Decode.decodeValue (Json.Decode.field "url" Json.Decode.string) request
        |> Result.map (\v -> "http://localhost" ++ v)
        |> Result.toMaybe
        |> Maybe.andThen Url.fromString
        |> Maybe.map .path
        |> Maybe.andThen Url.percentDecode
        |> Result.fromMaybe CannotParseUrl
        |> Result.andThen
            (\v ->
                if Regex.contains parentFolderRegex v then
                    Err ParentFolderPath

                else
                    Ok v
            )
        |> Result.map
            (\v ->
                if v |> String.endsWith "/" then
                    v ++ "index.html"

                else
                    v
            )
        |> Task.Extra.fromResult


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
        |> Task.mapError InternalError_


resolveQueue : Task.Task ElmServe.Error.Error ()
resolveQueue =
    JavaScript.run "(() => { if (!global.queue) global.queue = []; queue.forEach(a => a.res.end()); queue = []; })()"
        Json.Encode.null
        (Json.Decode.succeed ())
        |> Task.mapError ElmServe.Error.InternalError


sendFile : ElmServe.Options.Options -> String -> HttpServer.Request -> Task.Task JavaScript.Error ()
sendFile opt path { request, response } =
    JavaScript.run "require('send')(a.req, a.path, { root: a.root }).pipe(a.res)"
        (Json.Encode.object
            [ ( "root", Json.Encode.string opt.root )
            , ( "path", Json.Encode.string path )
            , ( "req", request )
            , ( "res", response )
            ]
        )
        (Json.Decode.succeed ())



--


exitWithMessageAndCode : String -> Int -> Task.Task ElmServe.Error.Error ()
exitWithMessageAndCode msg code =
    (Console.logError msg |> Task.mapError ElmServe.Error.ConsoleError)
        |> Task.andThen (\() -> Process.Extra.hardExit code |> Task.mapError ElmServe.Error.ExitError)



--


readFile : String -> Task.Task ElmServe.Error.Error String
readFile path =
    JavaScript.run "require('fs/promises').readFile(a, 'utf-8')"
        (Json.Encode.string path)
        Json.Decode.string
        |> Task.mapError ElmServe.Error.InternalError


writeFile : String -> String -> Task.Task ElmServe.Error.Error ()
writeFile path data =
    JavaScript.run "require('fs/promises').writeFile(a.path, a.data)"
        (Json.Encode.object
            [ ( "path", Json.Encode.string path )
            , ( "data", Json.Encode.string data )
            ]
        )
        (Json.Decode.succeed ())
        |> Task.mapError ElmServe.Error.InternalError



--


type FileStatus
    = File
    | Directory
    | NotFound


fileStatus : String -> Task.Task JavaScript.Error FileStatus
fileStatus path =
    JavaScript.run "require('fs/promises').stat(a).then(a => a.isDirectory())"
        (Json.Encode.string path)
        (Json.Decode.bool
            |> Json.Decode.andThen
                (\v ->
                    if v then
                        Json.Decode.succeed Directory

                    else
                        Json.Decode.succeed File
                )
        )
        |> Task.onError
            (\v ->
                case v of
                    JavaScript.Exception _ (JavaScript.ErrorCode "ENOENT") _ ->
                        Task.succeed NotFound

                    _ ->
                        Task.fail v
            )



--


decodeMsg : Json.Decode.Decoder ElmServe.Msg.Msg
decodeMsg =
    Json.Decode.field "a" Json.Decode.int
        |> Json.Decode.andThen
            (\a ->
                case a of
                    1 ->
                        Json.Decode.map ElmServe.Msg.FileChanged
                            (Json.Decode.field "b"
                                (Json.Decode.map (\v1 -> { path = v1 })
                                    (Json.Decode.field "path" Json.Decode.string)
                                )
                            )

                    3 ->
                        Json.Decode.map ElmServe.Msg.RequestReceived
                            (Json.Decode.field "b"
                                (Json.Decode.map2 ElmServe.Msg.Request
                                    (Json.Decode.field "req" Json.Decode.value)
                                    (Json.Decode.field "res" Json.Decode.value)
                                )
                            )

                    _ ->
                        Json.Decode.fail ("I can't decode \"Msg\", unknown variant with index " ++ String.fromInt a ++ ".")
            )
