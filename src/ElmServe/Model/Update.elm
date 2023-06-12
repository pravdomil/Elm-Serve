module ElmServe.Model.Update exposing (..)

import Console
import Dict
import Elm.Project
import ElmServe.Error
import ElmServe.Model
import ElmServe.Msg
import ElmServe.Options
import ElmServe.Utils.Utils
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
                            serverUrl : String
                            serverUrl =
                                HttpServer.url b.options.server

                            openServerUrl : Task.Task x ()
                            openServerUrl =
                                if b.options.open then
                                    ElmServe.Utils.Utils.open serverUrl
                                        |> Task.onError (\_ -> Task.succeed ())

                                else
                                    Task.succeed ()

                            task : Task.Task ElmServe.Error.Error ()
                            task =
                                (Console.log "Elm Serve\n\n" |> Task.mapError ElmServe.Error.ConsoleError)
                                    |> Task.andThen (\_ -> makeOutputFile b.options)
                                    |> Task.andThen (\_ -> startWatching b.project)
                                    |> Task.andThen (\_ -> HttpServer.start b.options.server)
                                    |> Task.andThen (\_ -> log ("Server is running at:\n" ++ serverUrl ++ "\n"))
                                    |> Task.andThen (\_ -> openServerUrl)
                        in
                        ( Ok b
                        , task
                            |> Task.attempt ElmServe.Msg.TaskDone
                        )

                    Err b ->
                        ( Err (ElmServe.Model.Error b)
                        , exitWithMessageAndCode (ElmServe.Error.toString b) 1
                            |> Task.attempt (\_ -> ElmServe.Msg.NothingHappened)
                        )

        ElmServe.Msg.GotFileChange _ ->
            \model ->
                let
                    killProcess : ElmServe.Model.Ready -> Task.Task x ()
                    killProcess b =
                        case b.compileProcess of
                            Just v ->
                                Process.kill v

                            Nothing ->
                                Task.succeed ()

                    task : Task.Task ElmServe.Error.Error ()
                    task =
                        Task.Extra.fromResult (Result.fromMaybe ElmServe.Error.InternalErrorModelNotReady model)
                            |> Task.andThen
                                (\c ->
                                    killProcess c
                                        |> Task.andThen (\_ -> Process.sleep 1)
                                        |> Task.andThen (\_ -> log "Recompiling...")
                                        |> Task.andThen (\_ -> makeOutputFile c.options)
                                        |> Task.andThen (\_ -> resolveQueue)
                                        |> Task.onError (\v -> exitWithMessageAndCode (ElmServe.Error.toString v) 1)
                                )
                in
                ( model
                , task
                    |> Process.spawn
                    |> Task.perform ElmServe.Msg.GotCompileProcess
                )

        ElmServe.Msg.GotCompileProcess a ->
            \model ->
                ( model |> Maybe.map (\v -> { v | compileProcess = Just a })
                , Cmd.none
                )

        ElmServe.Msg.GotRequest a ->
            \model ->
                let
                    task : Task.Task ElmServe.Error.Error ()
                    task =
                        Task.Extra.fromResult (Result.fromMaybe ElmServe.Error.InternalErrorModelNotReady model)
                            |> Task.andThen (\c -> sendResponse c.options a)
                in
                ( model
                , task
                    |> Task.attempt ElmServe.Msg.TaskDone
                )

        ElmServe.Msg.TaskDone a ->
            \model ->
                let
                    cmd : Cmd ElmServe.Msg.Msg
                    cmd =
                        case a of
                            Ok _ ->
                                Cmd.none

                            Err b ->
                                exitWithMessageAndCode (ElmServe.Error.toString b) 1
                                    |> Task.attempt (\_ -> ElmServe.Msg.TaskDone (Ok ()))
                in
                ( model
                , cmd
                )


subscriptions : ElmServe.Model.Model -> Sub ElmServe.Msg.Msg
subscriptions _ =
    sendMsgSubscription



--


port sendMsg : (Json.Decode.Value -> msg) -> Sub msg


sendMsgSubscription : Sub ElmServe.Msg.Msg
sendMsgSubscription =
    let
        decoder : Json.Decode.Value -> ElmServe.Msg.Msg
        decoder b =
            case Json.Decode.decodeValue decodeMsg b of
                Ok c ->
                    c

                Err c ->
                    ElmServe.Msg.TaskDone (Err (ElmServe.Error.InternalError (JavaScript.DecodeError c)))
    in
    sendMsg decoder



--


makeOutputFile : ElmServe.Options.Options -> Task.Task ElmServe.Error.Error ()
makeOutputFile opt =
    let
        recoverFromCompileError : ElmServe.Error.Error -> Task.Task ElmServe.Error.Error String
        recoverFromCompileError b =
            case b of
                ElmServe.Error.CannotCompileElm (JavaScript.Exception _ (JavaScript.ErrorCode "ENONZERO") msg) ->
                    (\(JavaScript.ErrorMessage v) -> v) msg
                        |> Json.Encode.string
                        |> Json.Encode.encode 0
                        |> (\v -> "elmServe.compileError(" ++ v ++ ");")
                        |> Task.succeed

                _ ->
                    Task.fail b
    in
    compileElm opt
        |> Task.andThen (\_ -> readFile opt.output)
        |> Task.andThen patchElm
        |> Task.onError recoverFromCompileError
        |> Task.andThen applyLib
        |> Task.andThen (writeFile opt.output)



--


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

        watch : String -> Task.Task JavaScript.Error ()
        watch b =
            JavaScript.run "require('fs').watch(a, { recursive: true }, (_, path) => scope.Elm.Main.init.ports.sendMsg.send({ a: 1, b: { path } }))"
                (Json.Encode.string b)
                (Json.Decode.succeed ())
    in
    dirs
        |> List.map watch
        |> Task.sequence
        |> Task.map (\_ -> ())
        |> Task.mapError ElmServe.Error.InternalError



--


type RespondError
    = CannotParseUrl
    | ParentFolderPath
    | NotFound_
    | InternalError_ JavaScript.Error


sendResponse : ElmServe.Options.Options -> ElmServe.Msg.Request -> Task.Task ElmServe.Error.Error ()
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


requestPath : ElmServe.Msg.Request -> Task.Task RespondError String
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


send : Int -> Dict.Dict String String -> String -> ElmServe.Msg.Request -> Task.Task JavaScript.Error ()
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


addRequestToQueue : ElmServe.Msg.Request -> Task.Task RespondError ()
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


sendFile : ElmServe.Options.Options -> String -> ElmServe.Msg.Request -> Task.Task JavaScript.Error ()
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
                        Json.Decode.map ElmServe.Msg.GotFileChange
                            (Json.Decode.field "b"
                                (Json.Decode.map (\v1 -> { path = v1 })
                                    (Json.Decode.field "path" Json.Decode.string)
                                )
                            )

                    3 ->
                        Json.Decode.map ElmServe.Msg.GotRequest
                            (Json.Decode.field "b"
                                (Json.Decode.map2 ElmServe.Msg.Request
                                    (Json.Decode.field "req" Json.Decode.value)
                                    (Json.Decode.field "res" Json.Decode.value)
                                )
                            )

                    _ ->
                        Json.Decode.fail ("I can't decode \"Msg\", unknown variant with index " ++ String.fromInt a ++ ".")
            )
