module Utils.Task_ exposing (..)

import Task exposing (Task)


fromResult : Result x a -> Task x a
fromResult a =
    case a of
        Ok b ->
            Task.succeed b

        Err b ->
            Task.fail b



--


apply : Task x a -> Task x (a -> b) -> Task x b
apply task a =
    Task.map2 (\fn v -> fn v) a task
