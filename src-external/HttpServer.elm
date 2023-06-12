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
        var callback = (req, res) => { scope.Elm.Main.init.ports.httpServer.send([req, res]) }
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



--


type alias Request =
    { request : Json.Decode.Value
    , response : Json.Decode.Value
    }



--


port httpServer : (Json.Decode.Value -> msg) -> Sub msg


requestSubscription : (Result Json.Decode.Error Request -> msg) -> Sub msg
requestSubscription fn =
    let
        decoder : Json.Decode.Decoder Request
        decoder =
            Json.Decode.map2
                Request
                (Json.Decode.index 0 Json.Decode.value)
                (Json.Decode.index 1 Json.Decode.value)
    in
    httpServer (\x -> fn (Json.Decode.decodeValue decoder x))
