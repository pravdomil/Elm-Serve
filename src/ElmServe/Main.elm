port module ElmServe.Main exposing (..)

import ElmServe.Options as Options exposing (Options)
import Interop.JavaScript as JavaScript
import Json.Decode as Decode
import Json.Encode as Encode
import Parser
import Parser.DeadEnd as DeadEnd
import Process
import Task exposing (Task)
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


type Msg
    = GotOptions (Result Error Options)
    | GotRequest Decode.Value
    | TaskDone (Result Error ())
    | NothingHappened


type Error
    = JavaScriptError JavaScript.Error
    | CannotParseOptions (List Parser.DeadEnd)


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

        GotRequest _ ->
            ( model
            , log "GotRequest"
                |> Task.attempt (always NothingHappened)
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
                        |> Task.attempt (always NothingHappened)
                    )

        NothingHappened ->
            ( model
            , Cmd.none
            )


errorToString : Error -> String
errorToString a =
    case a of
        JavaScriptError b ->
            JavaScript.errorToString b

        CannotParseOptions b ->
            "Cannot decoder options because:\n" ++ DeadEnd.toString b



--


port gotRequest : (Decode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    gotRequest GotRequest



--


startServer : Options -> Task Error ()
startServer a =
    JavaScript.run """
    (() => {
        var ssl = a.sslCert !== null && a.sslKey !== null
        var opt = ssl ? { cert: fs.readFileSync(a.sslCert), key: fs.readFileSync(a.sslKey) } : {}
        var callback = (req, res) => { scope.Elm.Main.init.ports.gotRequest.send({ req, res }) }

        global.static = require('serve-static')(a.root !== null ? a.root : process.cwd())
        global.server = require(ssl ? 'https' : 'http').createServer(opt, callback).listen(a.port, a.host)
    })()
    """
        (Encode.object
            [ ( "host", Encode.string a.host )
            , ( "port", Encode.int a.port_ )
            , ( "root", Encode_.maybe Encode.string a.root )
            , ( "sslCert", Encode_.maybe Encode.string a.sslCert )
            , ( "sslKey", Encode_.maybe Encode.string a.sslKey )
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


open : String -> Task Error ()
open _ =
    JavaScript.run "await require('open')(_v0)"
        |> Task.mapError JavaScriptError
        |> Task.map (\_ -> ())


exitWithMessageAndCode : String -> Int -> Task Error ()
exitWithMessageAndCode _ _ =
    JavaScript.run "(() => { console.error(_v0); process.exit(_v1); })()"
        |> Task.mapError JavaScriptError
        |> Task.map (\_ -> ())



--


serverUrl : Options -> String
serverUrl a =
    (if a.sslCert == Nothing then
        "http://"

     else
        "https://"
    )
        ++ a.host
        ++ ":"
        ++ String.fromInt a.port_
