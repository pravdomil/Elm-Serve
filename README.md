# Elm Serve

Alternative to [elm-live](https://github.com/wking-io/elm-live).

## Install

```sh
npm i pravdomil/elm-serve -g
```

## Usage

```
Welcome to Elm Serve.

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
        Serve index.html if page not found. Useful for Browser.application.

Elm Options:
    --elm <path>
        Set path to Elm compiler.

    --debug
        Turn on Elm debugger.

    --optimize
        Turn on Elm optimizations.

    --output <path>
        Set output from Elm compiler. Default is elm.js.
```
