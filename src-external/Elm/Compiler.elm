module Elm.Compiler exposing (..)

import JavaScript
import Process.Extra
import Task


type alias Options =
    { elmPath : String
    , debug : Bool
    , optimize : Bool
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
                :: (\acc ->
                        if opt.debug then
                            "--debug" :: acc

                        else
                            acc
                   )
                    ((\acc ->
                        if opt.optimize then
                            "--optimize" :: acc

                        else
                            acc
                     )
                        opt.input
                    )
    in
    Process.Extra.spawn opt.elmPath args
