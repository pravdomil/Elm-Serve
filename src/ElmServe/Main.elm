port module ElmServe.Main exposing (..)

import Dict exposing (Dict)
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
    { options : Maybe Options
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { options = Nothing
      }
    , getOptions
        |> Task.attempt GotOptions
    )


getOptions : Task Error Options
getOptions =
    getArguments
        |> Task.andThen
            (\v ->
                case Options.parse (List.drop 2 v) of
                    Ok vv ->
                        Task.succeed vv

                    Err vv ->
                        Task.fail (CannotParseOptions vv)
            )



--


type Error
    = JavaScriptError JavaScript.Error
    | CannotParseOptions (List Parser.DeadEnd)
    | GotRequestButOptionsAreNothing
    | GotRequestButWithNoRequestAndResponse


errorToString : Error -> String
errorToString a =
    case a of
        JavaScriptError b ->
            JavaScript.errorToString b

        CannotParseOptions b ->
            "Cannot decoder options because:\n" ++ DeadEnd.toString b

        GotRequestButOptionsAreNothing ->
            "Internal error - got request but options are nothing."

        GotRequestButWithNoRequestAndResponse ->
            "Internal error - got request but with no request and response."



--


type Msg
    = GotOptions (Result Error Options)
    | GotRequest (Result Decode.Error Request)
    | TaskDone (Result Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotOptions a ->
            case a of
                Ok b ->
                    ( { model | options = Just b }
                    , log ("Elm Serve\n\nI got following options:\n" ++ Options.toString b ++ "\n")
                        |> Task.andThen (\_ -> Process.sleep 1)
                        |> Task.andThen (\_ -> startServer b)
                        |> Task.andThen (\_ -> log ("Server is running at:\n" ++ serverUrl b ++ "\n"))
                        |> Task.andThen
                            (\_ ->
                                if b.open then
                                    open (serverUrl b)

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

        GotRequest a ->
            ( model
            , (case a of
                Ok b ->
                    case model.options of
                        Just c ->
                            sendResponse c b

                        Nothing ->
                            Task.fail GotRequestButOptionsAreNothing

                Err _ ->
                    Task.fail GotRequestButWithNoRequestAndResponse
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



--


startServer : Options -> Task Error ()
startServer a =
    JavaScript.run """
    (() => {
        var opt = a.ssl ? { cert: fs.readFileSync(a.sslCert), key: fs.readFileSync(a.sslKey) } : {}
        var callback = (req, res) => { scope.Elm.Main.init.ports.gotRequest.send({ req, res }) }

        global.serve = require('serve-static')(a.root !== null ? a.root : process.cwd())
        global.server = require(a.ssl ? 'https' : 'http').createServer(opt, callback).listen(a.port, a.host)
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
        |> Task.mapError JavaScriptError



--


type alias Request =
    { request : Decode.Value
    , response : Decode.Value
    }


port gotRequest : (Decode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    gotRequest
        (\v ->
            GotRequest
                (Result.map2 Request
                    (Decode.decodeValue (Decode.field "req" Decode.value) v)
                    (Decode.decodeValue (Decode.field "res" Decode.value) v)
                )
        )



--


type RespondError
    = CannotParseUrl
    | ParentFolderPath
    | NotFound_
    | JavaScriptError_ JavaScript.Error


sendResponse : Options -> Request -> Task Error ()
sendResponse opt a =
    let
        sendByPath : String -> Task RespondError ()
        sendByPath b =
            fileStatus (opt.root ++ "/" ++ b)
                |> Task.mapError JavaScriptError_
                |> Task.andThen
                    (\v ->
                        case v of
                            File ->
                                sendFile (opt.root ++ "/" ++ b)

                            Directory ->
                                redirect (b ++ "/")

                            NotFound ->
                                if opt.indexAs404 then
                                    sendFile (opt.root ++ "/index.html")

                                else
                                    Task.fail NotFound_
                    )

        sendFile : String -> Task RespondError ()
        sendFile b =
            Debug.todo ""

        redirect : String -> Task RespondError ()
        redirect b =
            send 301 (Dict.fromList [ ( "Location", b ) ]) ("Moved permanently to " ++ b ++ ".") a

        errorResponse : RespondError -> Task Error ()
        errorResponse b =
            case b of
                CannotParseUrl ->
                    send 400 Dict.empty "Bad request - cannot parse url." a

                ParentFolderPath ->
                    send 403 Dict.empty "Forbidden - cannot go to parent folder." a

                NotFound_ ->
                    send 404 Dict.empty "Not found." a

                JavaScriptError_ c ->
                    send 500 Dict.empty "Server error." a
                        |> Task.andThen (\_ -> Task.fail (JavaScriptError c))
    in
    requestPath a
        |> Task.andThen sendByPath
        |> Task.onError errorResponse


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


send : Int -> Dict String String -> String -> Request -> Task Error ()
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
        |> Task.mapError JavaScriptError



--


getArguments : Task Error (List String)
getArguments =
    JavaScript.run "process.argv"
        Encode.null
        (Decode.list Decode.string)
        |> Task.mapError JavaScriptError


log : String -> Task Error ()
log a =
    JavaScript.run "console.log(a)"
        (Encode.string a)
        (Decode.succeed ())
        |> Task.mapError JavaScriptError


exitWithMessageAndCode : String -> Int -> Task Error ()
exitWithMessageAndCode msg code =
    JavaScript.run "(() => { console.error(a.msg); process.exit(a.code); })()"
        (Encode.object
            [ ( "msg", Encode.string msg )
            , ( "code", Encode.int code )
            ]
        )
        (Decode.succeed ())
        |> Task.mapError JavaScriptError



--


open : String -> Task Error ()
open a =
    JavaScript.run "require('open')(a)"
        (Encode.string a)
        (Decode.succeed ())
        |> Task.mapError JavaScriptError


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


taskAndThenWithResult : (Result x a -> Task y b) -> Task x a -> Task y b
taskAndThenWithResult next a =
    a
        |> Task.map Ok
        |> Task.onError (Err >> Task.succeed)
        |> Task.andThen next
