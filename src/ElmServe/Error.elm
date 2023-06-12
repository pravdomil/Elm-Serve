module ElmServe.Error exposing (..)

import JavaScript
import Parser


type Error
    = CannotParseOptions (List Parser.DeadEnd)
    | CannotReadProject JavaScript.Error
      --
    | CannotCompileElm JavaScript.Error
    | CannotStartServer JavaScript.Error
    | CannotWatchFiles JavaScript.Error
    | CannotSendResponse JavaScript.Error
      --
    | InternalErrorModelNotReady
    | ConsoleError JavaScript.Error
    | ExitError JavaScript.Error


toString : Error -> String
toString a =
    let
        usage : String
        usage =
            """Welcome to Elm Serve.

Usage:
    elm-serve <elm-files>...

Options:
    --host <host>
        Set server host. Default is localhost.

    --port <port>
        Set server port. Default is 8000.

    --ssl <cert-file> <key-file>
        Turn on HTTPS.

    --root <path>
        Set server root.

    --open
        Open server URL in browser.

    --no-404
        Serve /index.html if page not found. Useful for Browser.application.

Elm Options:
    --elm <path>
        Set path to Elm compiler.

    --debug
        Turn on Elm debugger.

    --optimize
        Turn on Elm optimizations.

    --output <path>
        Set output from Elm compiler. Default is elm.js.
"""
    in
    case a of
        CannotParseOptions b ->
            case b |> List.head |> Maybe.map .problem of
                Just (Parser.Problem c) ->
                    c

                _ ->
                    usage

        CannotReadProject b ->
            case b of
                JavaScript.Exception _ (JavaScript.ErrorCode "ENOENT") _ ->
                    "Cannot find elm.json."

                _ ->
                    "Cannot read elm.json. " ++ JavaScript.errorToString b

        --
        CannotCompileElm b ->
            "Cannot compile Elm. " ++ JavaScript.errorToString b

        CannotStartServer b ->
            case b of
                JavaScript.Exception _ (JavaScript.ErrorCode "EADDRINUSE") _ ->
                    "There is somebody already listening on same port!"

                _ ->
                    "Cannot start server. " ++ JavaScript.errorToString b

        --
        InternalErrorModelNotReady ->
            "Internal error. Model is not ready."

        --
        InternalError b ->
            "Internal error. " ++ JavaScript.errorToString b
