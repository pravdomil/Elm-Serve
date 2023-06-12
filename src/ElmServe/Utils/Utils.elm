module ElmServe.Utils.Utils exposing (..)

import Elm.Project
import JavaScript
import Json.Decode
import Json.Encode
import Task


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


patchElm : String -> Task.Task JavaScript.Error String
patchElm a =
    JavaScript.run "require('./resources/patch.js').inject(a)"
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
                console.error('Elm Serve\\n', a)
                elmServe.showUi(a)
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
                elmServe.ui.textContent = a
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
                a.style.whiteSpace = 'pre'
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

        fetch("/elm-serve-client-lib.js")
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
