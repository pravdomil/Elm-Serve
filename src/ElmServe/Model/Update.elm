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
                            task : Task.Task ElmServe.Error.Error ()
                            task =
                                let
                                    opt : ElmServe.Options.Options
                                    opt =
                                        b.options

                                    openServerUrl : Task.Task ElmServe.Error.Error ()
                                    openServerUrl =
                                        if opt.open then
                                            open (serverUrl opt)

                                        else
                                            Task.succeed ()
                                in
                                log "Elm Serve\n\n"
                                    |> Task.andThen (\_ -> makeOutputFile opt)
                                    |> Task.andThen (\_ -> startWatching b.project)
                                    |> Task.andThen (\_ -> HttpServer.start opt)
                                    |> Task.andThen (\_ -> log ("Server is running at:\n" ++ serverUrl opt ++ "\n"))
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


compileElm : ElmServe.Options.Options -> Task.Task ElmServe.Error.Error String
compileElm opt =
    let
        args : List String
        args =
            [ "make"
            , "--output=" ++ opt.output
            ]
                ++ List.filterMap identity
                    [ boolToArg "--debug" opt.debug
                    , boolToArg "--optimize" opt.optimize
                    ]
                ++ opt.input

        boolToArg : String -> Bool -> Maybe String
        boolToArg name b =
            if b then
                Just name

            else
                Nothing
    in
    JavaScript.run """
    new Promise((resolve, reject) => {
        function err(code, msg) {
            var e = new Error(msg)
            e.code = code
            return e
        }

        var b = require('child_process').spawn(a.elm, a.args);
        var stdout = '';
        var stderr = '';
        b.on('error', reject)
        b.on('close', b => { b ? reject(err('ENONZERO', stderr)) : resolve(stdout) })
        b.stdout.on('data', b => { stdout += b })
        b.stderr.on('data', b => { stderr += b })
        onCancel(() => b.kill())
    })
    """
        (Json.Encode.object
            [ ( "elm", Json.Encode.string opt.elm )
            , ( "args", Json.Encode.list Json.Encode.string args )
            ]
        )
        Json.Decode.string
        |> Task.mapError ElmServe.Error.CannotCompileElm


patchElm : String -> Task.Task ElmServe.Error.Error String
patchElm a =
    JavaScript.run "require('elm-hot').inject(a)"
        (Json.Encode.string a)
        Json.Decode.string
        |> Task.mapError ElmServe.Error.InternalError


applyLib : String -> Task.Task ElmServe.Error.Error String
applyLib a =
    let
        lib : String
        lib =
            """
(function(){
    function init() {
        elmServe = {
            compileError: function(a) {
                console.error('Elm Serve\\n', a)
                elmServe.showUi(a)
            },
            disconnected: function() {
                var msg = 'We are disconnected.'
                console.error('Elm Serve\\n', msg)
                elmServe.showUi(msg)
            },
            clear: function() {
                elmServe.hideUi()
            },

            //

            showUi: function(a) {
                if (elmServe.ui.parentElement === null) {
                    if (document.body) document.body.appendChild(elmServe.ui)
                    else addEventListener("DOMContentLoaded", function() { document.body.appendChild(elmServe.ui) })
                }
                elmServe.ui.textContent = a
            },
            hideUi: function() {
                elmServe.ui.remove()
            },
            ui: (function() {
                a = document.createElement('div')
                a.id = 'elmServeUi'
                a.style.position = 'fixed'
                a.style.zIndex = '9999999'
                a.style.left = '0'
                a.style.right = '0'
                a.style.top = '0'
                a.style.bottom = '0'

                a.style.font = '14px/1 monospace'
                a.style.padding = '16px'
                a.style.whiteSpace = 'pre'
                a.style.color = 'white'
                a.style.backgroundColor = 'rgba(0, 0, 0, 0.6)'
                a.style.overflow = 'auto'

                return a
            })()
        }

        module = {
            hot: {
                data: null,
                verbose: false,
                disposeCallback : null,

                //

                accept: function () {},
                dispose: function (a) { module.hot.disposeCallback = a },
                apply: function () {
                    if (module.hot.disposeCallback === null) {
                        location.reload()
                        return
                    }
                    var data = {}
                    module.hot.disposeCallback(data)
                    module.hot.data = data
                    delete Elm
                }
            }
        }
    }

    function reload() {
        elmServe.clear()
        module.hot.apply()
    }

    function listen() {
        var src = document.currentScript.src

        function onLoad() {
            var script = document.createElement('script')
            script.src = src
            document.head.appendChild(script)
        }

        function onError() {
            elmServe.disconnected()
        }

        fetch("/elm-serve-client-lib.js")
            .then(onLoad)
            .catch(onError)
    }

    typeof elmServe === "undefined" ? init() : reload()
    listen()
})();
"""
    in
    (lib ++ a)
        |> Task.succeed



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


open : String -> Task.Task ElmServe.Error.Error ()
open a =
    JavaScript.run "require('open')(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())
        |> Task.mapError ElmServe.Error.InternalError



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


serverUrl : ElmServe.Options.Options -> String
serverUrl a =
    (if a.ssl == Nothing then
        "http://"

     else
        "https://"
    )
        ++ a.host
        ++ ":"
        ++ String.fromInt a.port_



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



--


decodeJson : Json.Decode.Decoder a -> Json.Decode.Decoder a
decodeJson decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\v ->
                case Json.Decode.decodeString decoder v of
                    Ok vv ->
                        Json.Decode.succeed vv

                    Err vv ->
                        Json.Decode.fail (Json.Decode.errorToString vv)
            )


encodeMaybe : (a -> Json.Encode.Value) -> (Maybe a -> Json.Encode.Value)
encodeMaybe encode a =
    case a of
        Just b ->
            encode b

        Nothing ->
            Json.Encode.null
