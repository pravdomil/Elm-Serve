module ElmServe.Path exposing (..)

import HttpServer
import Json.Decode
import Regex
import Url


type PathError
    = CannotParseUrl
    | ParentFolderPath


fromRequest : HttpServer.Request -> Result PathError String
fromRequest a =
    let
        parentFolderRegex : Regex.Regex
        parentFolderRegex =
            Regex.fromString "(^|/)\\.\\.(/|$)"
                |> Maybe.withDefault Regex.never
    in
    Json.Decode.decodeValue (Json.Decode.field "url" Json.Decode.string) a.request
        |> Result.map (\x -> "http://localhost" ++ x)
        |> Result.toMaybe
        |> Maybe.andThen Url.fromString
        |> Maybe.map .path
        |> Maybe.andThen Url.percentDecode
        |> Result.fromMaybe CannotParseUrl
        |> Result.andThen
            (\x ->
                if Regex.contains parentFolderRegex x then
                    Err ParentFolderPath

                else
                    Ok x
            )
        |> Result.map
            (\x ->
                if String.endsWith "/" x then
                    x ++ "index.html"

                else
                    x
            )
