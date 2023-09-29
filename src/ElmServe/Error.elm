module ElmServe.Error exposing (..)

import JavaScript
import Parser


type Error
    = CompileError JavaScript.Error
    | ServerError JavaScript.Error
    | WatchFilesError JavaScript.Error
    | ResponseError JavaScript.Error
    | QueueError JavaScript.Error
      --
    | ConsoleError JavaScript.Error
    | ExitError JavaScript.Error


toString : Error -> String
toString a =
    case a of
        CompileError b ->
            "Cannot compile Elm. " ++ JavaScript.errorToString b

        ServerError b ->
            case b of
                JavaScript.Exception _ (JavaScript.ErrorCode "EADDRINUSE") _ ->
                    "There is somebody already listening on same port!"

                _ ->
                    "Cannot start server. " ++ JavaScript.errorToString b

        WatchFilesError b ->
            "Internal error. " ++ JavaScript.errorToString b

        ResponseError b ->
            "Internal error. " ++ JavaScript.errorToString b

        QueueError b ->
            "Internal error. " ++ JavaScript.errorToString b

        --
        ConsoleError b ->
            "Internal error. " ++ JavaScript.errorToString b

        ExitError b ->
            "Internal error. " ++ JavaScript.errorToString b


usage : String
usage =
    """Elm Serve

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
