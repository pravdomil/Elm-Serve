port module FileWatch exposing (..)

import FileSystem
import JavaScript
import Json.Decode
import Json.Encode
import Task


watch : FileSystem.Path -> Task.Task JavaScript.Error ()
watch a =
    JavaScript.run "require('fs').watch(a, { recursive: true }, (_, b) => scope.Elm.Main.init.ports.fileWatch.send(b))"
        (Json.Encode.string (FileSystem.pathToString a))
        (Json.Decode.succeed ())



--


port fileWatch : (Json.Decode.Value -> msg) -> Sub msg


fileWatchSubscription : (Result Json.Decode.Error String -> msg) -> Sub msg
fileWatchSubscription fn =
    let
        decoder : Json.Decode.Decoder String
        decoder =
            Json.Decode.string
    in
    fileWatch (\x -> fn (Json.Decode.decodeValue decoder x))
