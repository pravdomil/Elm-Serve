port module ElmServe.Main exposing (..)

import Dict exposing (Dict)
import Elm.Project as Project exposing (Project)
import ElmServe.Options as Options exposing (Options)
import Interop.JavaScript as JavaScript
import Json.Decode as Decode
import Json.Encode as Encode
import Parser
import Parser.DeadEnd as DeadEnd
import Process
import Regex exposing (Regex)
import Task exposing (Task)
import Url exposing (Url)
import Utils.Json.Decode_ as Decode_
import Utils.Json.Encode_ as Encode_
import Utils.Task_ as Task_


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



--


type alias Model =
    Maybe ReadyModel


type alias ReadyModel =
    { options : Options
    , project : Project
    , compileProcess : Maybe Process.Id
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing
    , Task.map2
        (\v1 v2 ->
            { options = v1
            , project = v2
            , compileProcess = Nothing
            }
        )
        getOptions
        readProject
        |> Task.attempt GotReadyModel
    )


getOptions : Task Error Options
getOptions =
    JavaScript.run "process.argv"
        Encode.null
        (Decode.list Decode.string)
        |> Task.mapError InternalError
        |> Task.andThen
            (\v ->
                case Options.parse (List.drop 2 v) of
                    Ok vv ->
                        Task.succeed vv

                    Err vv ->
                        Task.fail (CannotParseOptions vv)
            )


readProject : Task Error Project
readProject =
    JavaScript.run "require('fs/promises').readFile('elm.json', 'utf-8')"
        Encode.null
        (Decode_.json Project.decoder)
        |> Task.mapError CannotReadProject



--


type Error
    = CannotParseOptions (List Parser.DeadEnd)
    | CannotReadProject JavaScript.Error
      --
    | CannotCompileElm JavaScript.Error
    | CannotStartServer JavaScript.Error
      --
    | InternalErrorModelNotReady
    | InternalError JavaScript.Error


errorToString : Error -> String
errorToString a =
    case a of
        CannotParseOptions b ->
            "Cannot parse options because:\n" ++ DeadEnd.toString b

        CannotReadProject b ->
            case b of
                JavaScript.Exception "ENOENT" _ ->
                    "Cannot find elm.json."

                _ ->
                    "Cannot read elm.json. " ++ JavaScript.errorToString b

        --
        CannotCompileElm b ->
            "Cannot compile Elm. " ++ JavaScript.errorToString b

        CannotStartServer b ->
            case b of
                JavaScript.Exception "EADDRINUSE" _ ->
                    "There is somebody already listening on same port!"

                _ ->
                    "Cannot start server. " ++ JavaScript.errorToString b

        --
        InternalErrorModelNotReady ->
            "Internal error. Model is not ready."

        --
        InternalError b ->
            "Internal error. " ++ JavaScript.errorToString b



--


type Msg
    = GotReadyModel (Result Error ReadyModel)
    | GotFileChange { path : String }
    | GotCompileProcess Process.Id
    | GotRequest Request
    | TaskDone (Result Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotReadyModel a ->
            let
                task : Task Error ()
                task =
                    case a of
                        Ok b ->
                            let
                                opt : Options
                                opt =
                                    b.options

                                openServerUrl : Task Error ()
                                openServerUrl =
                                    if opt.open then
                                        open (serverUrl opt)

                                    else
                                        Task.succeed ()
                            in
                            log ("Elm Serve\n\nI got following options:\n" ++ Options.toString opt ++ "\n")
                                |> Task.andThen (\_ -> makeOutputFile opt)
                                |> Task.andThen (\_ -> startWatching b.project)
                                |> Task.andThen (\_ -> startServer opt)
                                |> Task.andThen (\_ -> log ("Server is running at:\n" ++ serverUrl opt ++ "\n"))
                                |> Task.andThen (\_ -> openServerUrl)

                        Err b ->
                            Task.fail b
            in
            ( Result.toMaybe a
            , task
                |> Task.attempt TaskDone
            )

        GotFileChange _ ->
            let
                killProcess : ReadyModel -> Task x ()
                killProcess b =
                    case b.compileProcess of
                        Just v ->
                            Process.kill v

                        Nothing ->
                            Task.succeed ()

                task : Task Error ()
                task =
                    Task_.fromResult (Result.fromMaybe InternalErrorModelNotReady model)
                        |> Task.andThen
                            (\c ->
                                killProcess c
                                    |> Task.andThen (\_ -> Process.sleep 1)
                                    |> Task.andThen (\_ -> log "Recompiling...")
                                    |> Task.andThen (\_ -> makeOutputFile c.options)
                                    |> Task.andThen (\_ -> resolveQueue)
                                    |> Task.onError (\v -> exitWithMessageAndCode (errorToString v) 1)
                            )
            in
            ( model
            , task
                |> Process.spawn
                |> Task.perform GotCompileProcess
            )

        GotCompileProcess a ->
            ( model |> Maybe.map (\v -> { v | compileProcess = Just a })
            , Cmd.none
            )

        GotRequest a ->
            let
                task : Task Error ()
                task =
                    Task_.fromResult (Result.fromMaybe InternalErrorModelNotReady model)
                        |> Task.andThen (\c -> sendResponse c.options a)
            in
            ( model
            , task
                |> Task.attempt TaskDone
            )

        TaskDone a ->
            let
                cmd : Cmd Msg
                cmd =
                    case a of
                        Ok _ ->
                            Cmd.none

                        Err b ->
                            exitWithMessageAndCode (errorToString b) 1
                                |> Task.attempt (\_ -> TaskDone (Ok ()))
            in
            ( model
            , cmd
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    sendMsgSubscription



--


port sendMsg : (Decode.Value -> msg) -> Sub msg


sendMsgSubscription : Sub Msg
sendMsgSubscription =
    let
        decoder : Decode.Value -> Msg
        decoder b =
            case Decode.decodeValue decodeMsg b of
                Ok c ->
                    c

                Err c ->
                    TaskDone (Err (InternalError (JavaScript.DecodeError c)))
    in
    sendMsg decoder



--


makeOutputFile : Options -> Task Error ()
makeOutputFile opt =
    let
        recoverFromCompileError : Error -> Task Error String
        recoverFromCompileError b =
            case b of
                CannotCompileElm (JavaScript.Exception "ENONZERO" msg) ->
                    Encode.encode 0 (Encode.string msg)
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


compileElm : Options -> Task Error String
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
    new Promise((resolve, reject_) => {
        function reject(code, msg) {
            var e = new Error(msg)
            e.code = code
            reject_(e)
        }

        var elm = require('child_process').spawn(a.elmPath, a.args);
        var stdout = '';
        var stderr = '';
        elm.stdout.on('data', b => { stdout += b })
        elm.stderr.on('data', b => { stderr += b })
        elm.on('close', b => { b ? reject('ENONZERO', stderr) : resolve(stdout) })
        onCancel(() => elm.kill())
    })
    """
        (Encode.object
            [ ( "elmPath", Encode.string opt.elmPath )
            , ( "args", Encode.list Encode.string args )
            ]
        )
        Decode.string
        |> Task.mapError CannotCompileElm


patchElm : String -> Task Error String
patchElm a =
    JavaScript.run "require('elm-hot').inject(a)"
        (Encode.string a)
        Decode.string
        |> Task.mapError InternalError


applyLib : String -> Task Error String
applyLib a =
    let
        lib : String
        lib =
            """
if (typeof elmServe === "undefined") {
    console.info('Hello from Elm Serve!');
    var elmServe = {
        compileError: function(a) {
            console.error('Compile Error\\n', a)
        },
        disconnected: function() {
            console.error('We are disconnected.')
        },
        clear: function() {
            console.clear()
        }
    }
    var module = {
        hot: {
            accept: function () {},
            dispose: function (a) { module.hot.disposeCallback = a },
            data: null,
            apply: function () {
                var data = {}
                module.hot.disposeCallback(data)
                module.hot.data = data
                delete Elm
            },
            verbose: false,
            disposeCallback : null
        }
    }
} else {
    elmServe.clear()
    module.hot.apply()
}

(function(){
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
})();

"""
    in
    (lib ++ a)
        |> Task.succeed



--


startWatching : Project -> Task Error ()
startWatching a =
    let
        dirs : List String
        dirs =
            case a of
                Project.Application b ->
                    b.dirs

                Project.Package _ ->
                    [ "src" ]

        watch : String -> Task JavaScript.Error ()
        watch b =
            JavaScript.run "require('fs').watch(a, { recursive: true }, (_, path) => scope.Elm.Main.init.ports.sendMsg.send({ a: 1, b: { path } }))"
                (Encode.string b)
                (Decode.succeed ())
    in
    dirs
        |> List.map watch
        |> Task.sequence
        |> Task.map (\_ -> ())
        |> Task.mapError InternalError



--


startServer : Options -> Task Error ()
startServer a =
    JavaScript.run """
    new Promise((resolve, reject) => {
        var opt = a.ssl ? { cert: fs.readFileSync(a.sslCert), key: fs.readFileSync(a.sslKey) } : {}
        var callback = (req, res) => { scope.Elm.Main.init.ports.sendMsg.send({ a: 3, b: { req, res } }) }
        var server = require(a.ssl ? 'https' : 'http').createServer(opt, callback)
        server.on('error', reject)
        server.on('listening', resolve)
        server.listen(a.port, a.host)
    })
    """
        (Encode.object
            [ ( "host", Encode.string a.host )
            , ( "port", Encode.int a.port_ )
            , ( "root", Encode.string a.root )
            , ( "ssl", Encode.bool (Options.ssl a) )
            , ( "sslCert", Encode_.maybe Encode.string a.sslCert )
            , ( "sslKey", Encode_.maybe Encode.string a.sslKey )
            ]
        )
        (Decode.succeed ())
        |> Task.mapError CannotStartServer


type alias Request =
    { request : Decode.Value
    , response : Decode.Value
    }



--


type RespondError
    = CannotParseUrl
    | ParentFolderPath
    | NotFound_
    | InternalError_ JavaScript.Error


sendResponse : Options -> Request -> Task Error ()
sendResponse opt a =
    let
        resolvePath : String -> Task RespondError ()
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
                                    if opt.indexAs404 then
                                        sendFile opt "index.html" a
                                            |> Task.mapError InternalError_

                                    else
                                        Task.fail NotFound_
                        )

        redirect : String -> Task RespondError ()
        redirect b =
            send 301 (Dict.fromList [ ( "Location", b ) ]) ("Moved permanently to " ++ b ++ ".") a
                |> Task.mapError InternalError_

        errorResponse : RespondError -> Task JavaScript.Error ()
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
        |> Task.mapError InternalError


requestPath : Request -> Task RespondError String
requestPath { request } =
    let
        parentFolderRegex : Regex
        parentFolderRegex =
            Regex.fromString "(^|/)\\.\\.(/|$)"
                |> Maybe.withDefault Regex.never
    in
    Decode.decodeValue (Decode.field "url" Decode.string) request
        |> Result.map (\v -> "http://localhost" ++ v)
        |> Result.toMaybe
        |> Maybe.andThen Url.fromString
        |> Maybe.map .path
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
        |> Task_.fromResult


send : Int -> Dict String String -> String -> Request -> Task JavaScript.Error ()
send status headers data { response } =
    JavaScript.run "a.res.writeHead(a.status, a.headers).end(a.data)"
        (Encode.object
            [ ( "status", Encode.int status )
            , ( "headers", Encode.dict identity Encode.string headers )
            , ( "data", Encode.string data )
            , ( "res", response )
            ]
        )
        (Decode.succeed ())


addRequestToQueue : Request -> Task RespondError ()
addRequestToQueue a =
    JavaScript.run "(() => { if (!global.queue) global.queue = []; queue.push(a); })()"
        (Encode.object
            [ ( "req", a.request )
            , ( "res", a.response )
            ]
        )
        (Decode.succeed ())
        |> Task.mapError InternalError_


resolveQueue : Task Error ()
resolveQueue =
    JavaScript.run "(() => { if (!global.queue) global.queue = []; queue.forEach(a => a.res.end()); queue = []; })()"
        Encode.null
        (Decode.succeed ())
        |> Task.mapError InternalError


sendFile : Options -> String -> Request -> Task JavaScript.Error ()
sendFile opt path { request, response } =
    JavaScript.run "require('send')(a.req, a.path, { root: a.root }).pipe(a.res)"
        (Encode.object
            [ ( "root", Encode.string opt.root )
            , ( "path", Encode.string path )
            , ( "req", request )
            , ( "res", response )
            ]
        )
        (Decode.succeed ())



--


log : String -> Task Error ()
log a =
    JavaScript.run "console.log(a)"
        (Encode.string a)
        (Decode.succeed ())
        |> Task.mapError InternalError


exitWithMessageAndCode : String -> Int -> Task Error ()
exitWithMessageAndCode msg code =
    JavaScript.run "(() => { console.error(a.msg); process.exit(a.code); })()"
        (Encode.object
            [ ( "msg", Encode.string msg )
            , ( "code", Encode.int code )
            ]
        )
        (Decode.succeed ())
        |> Task.mapError InternalError



--


open : String -> Task Error ()
open a =
    JavaScript.run "require('open')(a)"
        (Encode.string a)
        (Decode.succeed ())
        |> Task.mapError InternalError



--


readFile : String -> Task Error String
readFile path =
    JavaScript.run "require('fs/promises').readFile(a, 'utf-8')"
        (Encode.string path)
        Decode.string
        |> Task.mapError InternalError


writeFile : String -> String -> Task Error ()
writeFile path data =
    JavaScript.run "require('fs/promises').writeFile(a.path, a.data)"
        (Encode.object
            [ ( "path", Encode.string path )
            , ( "data", Encode.string data )
            ]
        )
        (Decode.succeed ())
        |> Task.mapError InternalError



--


type FileStatus
    = File
    | Directory
    | NotFound


fileStatus : String -> Task JavaScript.Error FileStatus
fileStatus path =
    JavaScript.run "require('fs/promises').stat(a).then(a => a.isDirectory())"
        (Encode.string path)
        (Decode.bool
            |> Decode.andThen
                (\v ->
                    if v then
                        Decode.succeed Directory

                    else
                        Decode.succeed File
                )
        )
        |> Task.onError
            (\v ->
                case v of
                    JavaScript.Exception "ENOENT" _ ->
                        Task.succeed NotFound

                    _ ->
                        Task.fail v
            )



--


serverUrl : Options -> String
serverUrl a =
    (if Options.ssl a then
        "https://"

     else
        "http://"
    )
        ++ a.host
        ++ ":"
        ++ String.fromInt a.port_



--


decodeMsg : Decode.Decoder Msg
decodeMsg =
    Decode.field "a" Decode.int
        |> Decode.andThen
            (\a ->
                case a of
                    1 ->
                        Decode.map GotFileChange
                            (Decode.field "b"
                                (Decode.map (\v1 -> { path = v1 })
                                    (Decode.field "path" Decode.string)
                                )
                            )

                    3 ->
                        Decode.map GotRequest
                            (Decode.field "b"
                                (Decode.map2 Request
                                    (Decode.field "req" Decode.value)
                                    (Decode.field "res" Decode.value)
                                )
                            )

                    _ ->
                        Decode.fail ("I can't decode \"Msg\", unknown variant with index " ++ String.fromInt a ++ ".")
            )
