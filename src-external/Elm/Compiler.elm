module Elm.Compiler exposing (..)

import JavaScript
import Process.Extra
import Task


type alias Options =
    { elmPath : String
    , debug : Bool
    , optimize : Bool
    , jsonReport : Bool
    , input : List String
    , output : String
    }


compile : Options -> Task.Task JavaScript.Error String
compile opt =
    let
        args : List String
        args =
            "make"
                :: ("--output=" ++ opt.output)
                :: (\x ->
                        if opt.debug then
                            "--debug" :: x

                        else
                            x
                   )
                    ((\x ->
                        if opt.optimize then
                            "--optimize" :: x

                        else
                            x
                     )
                        ((\x ->
                            if opt.jsonReport then
                                "--report=json" :: x

                            else
                                x
                         )
                            opt.input
                        )
                    )
    in
    Process.Extra.spawn opt.elmPath args
