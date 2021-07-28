port module ElmServe.Main exposing (..)

import ElmServe.Options as Options exposing (Options)
import Interop.JavaScript as JavaScript
import Json.Decode as Decode
import Parser
import Parser.DeadEnd as DeadEnd
import Process
import Task exposing (Task)


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
    let
        getOptions : Cmd Msg
        getOptions =
            JavaScript.run "process.argv"
                |> JavaScript.decode (Decode.list Decode.string)
                |> Task.mapError JavaScriptError
                |> Task.andThen
                    (\v ->
                        case Options.parse (List.drop 2 v) of
                            Ok vv ->
                                Task.succeed vv

                            Err vv ->
                                Task.fail (CannotParseOptions vv)
                    )
                |> Task.attempt GotOptions
    in
    ( { options = Nothing
      }
    , getOptions
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
                    , log ("Elm Serve\n\nGot following options:\n" ++ Options.toString b ++ "\n")
                        |> Task.andThen (\_ -> Process.sleep 1)
                        |> Task.andThen (\_ -> startServer b)
                        |> Task.andThen (\_ -> log ("Server is running at:\n" ++ serverUrl b ++ "\n\n"))
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
    let
        _ =
            a.host

        _ =
            a.port_

        _ =
            a.root

        _ =
            a.sslCert

        _ =
            a.sslKey
    in
    JavaScript.run """
    (() => {
        var isJust = (a) => a.$ === 0
        var opt = isJust(_v3) && isJust(_v4) ? { cert: fs.readFileSync(_v3.a), key: fs.readFileSync(_v4.a) } : {}
        var callback = (req, res) => { scope.Elm.Main.init.ports.gotRequest.send({ req, res }) }

        global.static = require('serve-static')(isJust(_v2) ? _v2.a : process.cwd())
        global.server = require(isJust(_v3) ? 'https' : 'http').createServer(opt, callback).listen(_v1, _v0)
    })()
    """
        |> Task.mapError JavaScriptError
        |> Task.map (\_ -> ())



--


log : String -> Task Error ()
log _ =
    JavaScript.run "console.log(_v0)"
        |> Task.mapError JavaScriptError
        |> Task.map (\_ -> ())


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
