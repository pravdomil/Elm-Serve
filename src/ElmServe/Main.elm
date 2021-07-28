module ElmServe.Main exposing (..)

import ElmServe.Options as Options exposing (Options)
import Interop.JavaScript as JavaScript
import Json.Decode as Decode
import Parser
import Parser.DeadEnd as DeadEnd
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
        getArgs : Cmd Msg
        getArgs =
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
    , getArgs
    )



--


type Msg
    = GotOptions (Result Error Options)
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
                    , log ("Got following options:\n" ++ Options.toString b)
                        |> Task.attempt TaskDone
                    )

                Err b ->
                    ( model
                    , Task.fail b
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
