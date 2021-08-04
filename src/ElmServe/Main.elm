port module ElmServe.Main exposing (..)

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
            , ( "root", Encode_.maybe Encode.string a.root )
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
    | JavaScriptError_ JavaScript.Error


sendResponse : Options -> Request -> Task Error ()
sendResponse opt a =
    let
        parseUrl : Request -> Task RespondError String
        parseUrl { request } =
            Decode.decodeValue (Decode.field "url" Decode.string) request
                |> Result.map (\v -> "http://localhost" ++ v)
                |> Result.toMaybe
                |> Maybe.andThen Url.fromString
                |> Maybe.map .path
                |> Maybe.map
                    (\v ->
                        if v |> String.endsWith "/" then
                            v ++ "index.html"

                        else
                            v
                    )
                |> Result.fromMaybe CannotParseUrl
                |> resultToTask

        parentFolderPathCheck : String -> Task RespondError String
        parentFolderPathCheck b =
            let
                parentFolderRegex : Regex
                parentFolderRegex =
                    Regex.fromString "(^|/)\\.\\.(/|$)"
                        |> Maybe.withDefault Regex.never
            in
            if Regex.contains parentFolderRegex b then
                Task.fail ParentFolderPath

            else
                Task.succeed b

        sendResponse_ : Result RespondError () -> Task Error ()
        sendResponse_ b =
            case b of
                Ok _ ->
                    send 200 "Hello word." a

                Err c ->
                    case c of
                        CannotParseUrl ->
                            send 400 "Bad request - cannot parse url." a

                        ParentFolderPath ->
                            send 403 "Forbidden - cannot go to parent folder." a

                        JavaScriptError_ d ->
                            send 500 "Server error." a
                                |> Task.andThen (\_ -> Task.fail (JavaScriptError d))
    in
    parseUrl a
        |> Task.andThen parentFolderPathCheck
        |> taskAndThenWithResult sendResponse_


send : Int -> String -> Request -> Task Error ()
send status data { response } =
    JavaScript.run "(() => { a.res.statusCode = a.status; a.res.end(a.data); })()"
        (Encode.object
            [ ( "status", Encode.int status )
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


fileStatus : String -> Task Error FileStatus
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
        |> Task.mapError JavaScriptError



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
