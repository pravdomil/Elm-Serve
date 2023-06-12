module FileStatus exposing (..)

import FileSystem
import JavaScript
import Json.Decode
import Json.Encode
import Task


type FileStatus
    = File
    | Directory
    | NotFound


fileStatus : FileSystem.Path -> Task.Task JavaScript.Error FileStatus
fileStatus path =
    JavaScript.run "require('fs/promises').stat(a).then(a => a.isDirectory())"
        (Json.Encode.string (FileSystem.pathToString path))
        (Json.Decode.bool
            |> Json.Decode.andThen
                (\x ->
                    if x then
                        Json.Decode.succeed Directory

                    else
                        Json.Decode.succeed File
                )
        )
        |> Task.onError
            (\x ->
                case x of
                    JavaScript.Exception _ (JavaScript.ErrorCode "ENOENT") _ ->
                        Task.succeed NotFound

                    _ ->
                        Task.fail x
            )
