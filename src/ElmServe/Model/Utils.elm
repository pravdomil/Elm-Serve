module ElmServe.Model.Utils exposing (..)

import Elm.Project
import ElmServe.Model
import JavaScript
import Json.Decode
import Json.Encode
import Process.Extra
import Task


errorToString : ElmServe.Model.Error -> String
errorToString a =
    case a of
        ElmServe.Model.CompileError b ->
            "Cannot compile Elm. " ++ JavaScript.errorToString b

        ElmServe.Model.ServerError b ->
            case b of
                JavaScript.Exception _ (JavaScript.ErrorCode "EADDRINUSE") _ ->
                    "There is somebody already listening on same port!"

                _ ->
                    "Cannot start server. " ++ JavaScript.errorToString b

        ElmServe.Model.WatchFilesError b ->
            "Internal error. " ++ JavaScript.errorToString b

        ElmServe.Model.ResponseError b ->
            "Internal error. " ++ JavaScript.errorToString b

        ElmServe.Model.QueueError b ->
            "Internal error. " ++ JavaScript.errorToString b

        --
        ElmServe.Model.ConsoleError b ->
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



--


readProject : String -> Task.Task JavaScript.Error Elm.Project.Project
readProject a =
    JavaScript.run "require('fs/promises').readFile(a, 'utf-8')"
        (Json.Encode.string a)
        (decodeJson Elm.Project.decoder)


decodeJson : Json.Decode.Decoder a -> Json.Decode.Decoder a
decodeJson decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\x ->
                case Json.Decode.decodeString decoder x of
                    Ok x2 ->
                        Json.Decode.succeed x2

                    Err x2 ->
                        Json.Decode.fail (Json.Decode.errorToString x2)
            )



--


open : String -> Task.Task JavaScript.Error ()
open a =
    JavaScript.run "require('open')(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())



--


elmFfi : String -> Task.Task JavaScript.Error String
elmFfi a =
    Process.Extra.spawn "elm-ffi" [ a ]



--


elmHot : String -> Task.Task JavaScript.Error String
elmHot a =
    JavaScript.run "require('../resources/patch.js').inject(a)"
        (Json.Encode.string a)
        Json.Decode.string



--


jsLibrary : String
jsLibrary =
    """
(function(){

function init() {
    elmServe = {
        compileError: function(a) {
            var fn = function (b) {
                return typeof b === "string" ? b : b.string
            }
            try {
                var b = JSON.parse(a)
                b.errors.forEach(c => c.problems.forEach(d =>
                    console.error(c.name + " " + d.title + "\\n", d.message.map(fn).join("\\n"))
                ))
                elmServe.showUi(b.errors.map(c => c.problems.map(d =>
                     c.name + " " + d.title + "\\n\\n" + d.message.map(fn).join("\\n").split("\\n").map(e => "\\t" + e).join("\\n")
                ).join("\\n\\n\\n\\n")).join("\\n\\n\\n\\n"))
            } catch (e) {
                console.error('Elm Serve\\n', a)
                elmServe.showUi(a)
            }
        },
        disconnected: function() {
            var msg = 'We are disconnected.'
            console.error('Elm Serve\\n', msg)
            elmServe.showUi(msg)
        },
        clear: function() {
            elmServe.hideUi()
        },

        //

        showUi: function(a) {
            if (elmServe.ui.parentElement === null) {
                if (document.body) document.body.appendChild(elmServe.ui)
                else addEventListener("DOMContentLoaded", function() { document.body.appendChild(elmServe.ui) })
            }
            elmServe.ui.replaceChildren(a)
        },
        hideUi: function() {
            elmServe.ui.remove()
        },
        ui: (function() {
            a = document.createElement('div')
            a.id = 'elmServeUi'
            a.style.position = 'fixed'
            a.style.zIndex = '9999999'
            a.style.left = '0'
            a.style.right = '0'
            a.style.top = '0'
            a.style.bottom = '0'

            a.style.font = '14px/1 monospace'
            a.style.padding = '16px'
            a.style.whiteSpace = 'pre-wrap'
            a.style.color = 'white'
            a.style.backgroundColor = 'rgba(0, 0, 0, 0.6)'
            a.style.overflow = 'auto'

            return a
        })()
    }

    module = {
        hot: {
            data: null,
            verbose: false,
            disposeCallback : null,

            //

            accept: function () {},
            dispose: function (a) { module.hot.disposeCallback = a },
            apply: function () {
                if (module.hot.disposeCallback === null) {
                    location.reload()
                    return
                }
                var data = {}
                module.hot.disposeCallback(data)
                module.hot.data = data
                delete Elm
            }
        }
    }
}

function reload() {
    elmServe.clear()
    module.hot.apply()
}

function listen() {
    var src = document.currentScript.src

    function onLoad() {
        var script = document.createElement('script')
        script.src = src
        document.head.appendChild(script)
    }

    function onError() {
        elmServe.disconnected()
    }

    fetch(src + "/../elm-serve-client-lib.js")
        .then(onLoad)
        .catch(onError)
}

typeof elmServe === "undefined" ? init() : reload()
listen()

})();
"""


reportCompileErrorJs : String -> String
reportCompileErrorJs a =
    "elmServe.compileError(" ++ Json.Encode.encode 0 (Json.Encode.string a) ++ ");"
