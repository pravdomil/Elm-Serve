port module ElmServe.Main exposing (..)

import ElmServe.Model
import ElmServe.Model.Update
import ElmServe.Msg


main : Program () ElmServe.Model.Model ElmServe.Msg.Msg
main =
    Platform.worker
        { init = ElmServe.Model.Update.init
        , update = ElmServe.Model.Update.update
        , subscriptions = ElmServe.Model.Update.subscriptions
        }
