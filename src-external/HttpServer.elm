port module HttpServer exposing (..)

import JavaScript
import Json.Decode
import Json.Encode
import Task


type alias Options =
    { host : String
    , port_ : Int
    , ssl : Maybe { cert : String, key : String }
    }


start : Options -> Task.Task JavaScript.Error ()
start a =
    JavaScript.run """
    new Promise((resolve, reject) => {
        var opt = a[2] ? { cert: fs.readFileSync(a[2][0]), key: fs.readFileSync(a[2][1]) } : {}
        var callback = (req, res) => { scope.Elm.Main.init.ports.sendMsg.send({ a: 3, b: { req, res } }) }
        var server = require(a[2] ? 'https' : 'http').createServer(opt, callback)
        server.on('error', reject)
        server.on('listening', resolve)
        server.listen(a[1], a[0])
    })
    """
        (Json.Encode.list identity
            [ Json.Encode.string a.host
            , Json.Encode.int a.port_
            , case a.ssl of
                Just b ->
                    Json.Encode.list Json.Encode.string [ b.cert, b.key ]

                Nothing ->
                    Json.Encode.null
            ]
        )
        (Json.Decode.succeed ())


url : Options -> String
url a =
    (if a.ssl == Nothing then
        "http://"

     else
        "https://"
    )
        ++ a.host
        ++ ":"
        ++ String.fromInt a.port_
