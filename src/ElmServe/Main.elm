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


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



--


type alias Model =
    Maybe
        { options : Options
        , project : Project
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing
    , Task.map2
        (\v1 v2 ->
            { options = v1
            , project = v2
            }
        )
        getOptions
        readProject
        |> Task.attempt GotModel
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
      --
    | CannotDecodeRequest Decode.Error
    | GotRequestButModelIsNothing
      --
    | CannotDecodeFileChange Decode.Error
    | GotFileChangeButModelIsNothing
      --
    | InternalError JavaScript.Error


errorToString : Error -> String
errorToString a =
    case a of
        CannotParseOptions b ->
            "Cannot parse options because:\n" ++ DeadEnd.toString b

        CannotReadProject b ->
            case b of
                JavaScript.FileNotPatched ->
                    JavaScript.errorToString b

                JavaScript.Exception _ ->
                    if JavaScript.errorCode b == Just "ENOENT" then
                        "Cannot find elm.json."

                    else
                        JavaScript.errorToString b

                JavaScript.DecodeError _ ->
                    "Cannot decode elm.json."

        --
        CannotCompileElm b ->
            "Cannot compile Elm. " ++ JavaScript.errorToString b

        --
        CannotDecodeRequest _ ->
            "Internal error. Cannot decode request."

        GotRequestButModelIsNothing ->
            "Internal error. Got request but model is nothing."

        --
        CannotDecodeFileChange _ ->
            "Internal error. Cannot file change."

        GotFileChangeButModelIsNothing ->
            "Internal error. Got file change but model is nothing."

        --
        InternalError b ->
            "Internal error. " ++ JavaScript.errorToString b



--


type Msg
    = GotModel (Result Error { options : Options, project : Project })
    | GotFileChange (Result Decode.Error String)
    | GotRequest (Result Decode.Error Request)
    | TaskDone (Result Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotModel a ->
            case a of
                Ok b ->
                    let
                        opt : Options
                        opt =
                            b.options
                    in
                    ( Just b
                    , log ("Elm Serve\n\nI got following options:\n" ++ Options.toString opt ++ "\n")
                        |> Task.andThen (\_ -> makeOutputFile opt)
                        |> Task.andThen (\_ -> startWatching b.project)
                        |> Task.andThen (\_ -> startServer opt)
                        |> Task.andThen (\_ -> log ("Server is running at:\n" ++ serverUrl opt ++ "\n"))
                        |> Task.andThen
                            (\_ ->
                                if opt.open then
                                    open (serverUrl opt)

                                else
                                    Task.succeed ()
                            )
                        |> Task.attempt TaskDone
                    )

                Err b ->
                    ( model
                    , Task.fail b
                        |> Task.attempt TaskDone
                    )

        GotFileChange a ->
            ( model
            , (case a of
                Ok b ->
                    case model of
                        Just _ ->
                            log ("File " ++ b ++ " changed.")

                        Nothing ->
                            Task.fail GotFileChangeButModelIsNothing

                Err b ->
                    Task.fail (CannotDecodeFileChange b)
              )
                |> Task.attempt TaskDone
            )

        GotRequest a ->
            ( model
            , (case a of
                Ok b ->
                    case model of
                        Just c ->
                            sendResponse c.options b

                        Nothing ->
                            Task.fail GotRequestButModelIsNothing

                Err b ->
                    Task.fail (CannotDecodeRequest b)
              )
                |> Task.attempt TaskDone
            )

        TaskDone a ->
            case a of
                Ok _ ->
                    ( model
                    , Cmd.none
                    )

                Err b ->
                    ( model
                    , exitWithMessageAndCode (errorToString b) 1
                        |> Task.attempt (\_ -> TaskDone (Ok ()))
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ gotFileChange
            (\v ->
                GotFileChange (Decode.decodeValue Decode.string v)
            )
        , gotRequest
            (\v ->
                GotRequest
                    (Decode.decodeValue
                        (Decode.map2 Request
                            (Decode.field "req" Decode.value)
                            (Decode.field "res" Decode.value)
                        )
                        v
                    )
            )
        ]



--


makeOutputFile : Options -> Task Error ()
makeOutputFile opt =
    compileElm opt
        |> Task.andThen (\_ -> readFile opt.output)
        |> Task.andThen patchElm
        |> Task.andThen patchLibs
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
    new Promise((resolve, reject) => {
        var elm = require('child_process').spawn(a.elmPath, a.args);
        var stdout = '';
        var stderr = '';
        elm.stdout.on('data', b => { stdout += b })
        elm.stderr.on('data', b => { stderr += b })
        elm.on('close', b => {
          if (b) { var e = new Error(stderr); e.code = 'NONZERO'; reject(e); }
          else { resolve(stdout); }
        })
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


patchLibs : String -> Task Error String
patchLibs a =
    let
        elmServeLib : String
        elmServeLib =
            """
console.log('Hello from Elm Serve!');
"""

        moduleLib : String
        moduleLib =
            """
// https://github.com/klazuka/elm-hot/blob/fb2dc49e9b4fa53b51fa6088a1ac7ffa0b72557a/test/client.js#L38
var module = {
    hot: {
        accept: function () {},
        dispose: function (a) { module.hot.disposeCallback = a },
        data: null,
        apply: function () {
            var data = {}
            module.hot.disposeCallback(data)
            module.hot.data = data
        },
        verbose: true,
        disposeCallback : null
    }
};
"""
    in
    (elmServeLib ++ moduleLib ++ a)
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
            JavaScript.run "require('fs').watch(a, { recursive: true }, (_, file) => scope.Elm.Main.init.ports.gotFileChange.send(file))"
                (Encode.string b)
                (Decode.succeed ())
    in
    dirs
        |> List.map watch
        |> Task.sequence
        |> Task.map (\_ -> ())
        |> Task.mapError InternalError


port gotFileChange : (Decode.Value -> msg) -> Sub msg



--


startServer : Options -> Task Error ()
startServer a =
    JavaScript.run """
    (() => {
        var opt = a.ssl ? { cert: fs.readFileSync(a.sslCert), key: fs.readFileSync(a.sslKey) } : {}
        var callback = (req, res) => { scope.Elm.Main.init.ports.gotRequest.send({ req, res }) }

        require(a.ssl ? 'https' : 'http')
          .createServer(opt, callback)
          .listen(a.port, a.host)
    })()
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
        |> Task.mapError InternalError



--


type alias Request =
    { request : Decode.Value
    , response : Decode.Value
    }


port gotRequest : (Decode.Value -> msg) -> Sub msg



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
                sendClientLib a

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
        |> resultToTask


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


sendClientLib : Request -> Task RespondError ()
sendClientLib a =
    let
        body : String
        body =
            "console.log('Hello from Elm Serve.')"
    in
    send 200 Dict.empty body a
        |> Task.mapError InternalError_


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
                if JavaScript.errorCode v == Just "ENOENT" then
                    Task.succeed NotFound

                else
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


resultToTask : Result x a -> Task x a
resultToTask a =
    case a of
        Ok b ->
            Task.succeed b

        Err b ->
            Task.fail b
