module ElmServe.Options exposing (..)

import Parser as P exposing ((|.), (|=), Parser)


type alias Options =
    { host : String
    , port_ : Int

    --
    , root : String
    , indexAs404 : Bool
    , open : Bool

    --
    , sslCert : Maybe String
    , sslKey : Maybe String

    --
    , elmPath : String
    , debug : Bool
    , optimize : Bool
    , input : List String
    , output : String
    }



--


toString : Options -> String
toString a =
    let
        boolToString : Bool -> String
        boolToString b =
            if b then
                "true"

            else
                "false"
    in
    [ "Host:         " ++ a.host
    , "Port:         " ++ String.fromInt a.port_

    --
    , "Root:         " ++ a.root
    , "Index As 404: " ++ (a.indexAs404 |> boolToString)
    , "Open:         " ++ (a.open |> boolToString)

    --
    , "SSL Cert:     " ++ (a.sslCert |> Maybe.withDefault "-")
    , "SSL Key:      " ++ (a.sslKey |> Maybe.withDefault "-")

    --
    , "Elm Path:     " ++ a.elmPath
    , "Debug:        " ++ (a.debug |> boolToString)
    , "Optimize:     " ++ (a.optimize |> boolToString)
    , "Input:        " ++ (a.input |> String.join ", ")
    , "Output:       " ++ a.output
    ]
        |> String.join "\n"


ssl : Options -> Bool
ssl a =
    a.sslCert /= Nothing && a.sslKey /= Nothing



--


parse : List String -> Result (List P.DeadEnd) Options
parse a =
    String.join "\u{0000}" a
        |> P.run parser


parser : Parser Options
parser =
    let
        loop : Options -> Parser (P.Step Options Options)
        loop acc =
            P.oneOf
                [ P.succeed (\v -> P.Loop { acc | host = v })
                    |= stringArg "host"
                , P.succeed (\v -> P.Loop { acc | port_ = v })
                    |= intArg "port"

                --
                , P.succeed (\v -> P.Loop { acc | root = v })
                    |= stringArg "root"
                , P.succeed (\v -> P.Loop { acc | indexAs404 = v })
                    |= boolArg "index-as-404"
                , P.succeed (\v -> P.Loop { acc | open = v })
                    |= boolArg "open"

                --
                , P.succeed (\v -> P.Loop { acc | sslCert = Just v })
                    |= stringArg "ssl-cert"
                , P.succeed (\v -> P.Loop { acc | sslKey = Just v })
                    |= stringArg "ssl-key"

                --
                , P.succeed (\v -> P.Loop { acc | elmPath = v })
                    |= stringArg "elm-path"
                , P.succeed (\v -> P.Loop { acc | debug = v })
                    |= boolArg "debug"
                , P.succeed (\v -> P.Loop { acc | optimize = v })
                    |= boolArg "optimize"
                , P.succeed (\v -> P.Loop { acc | output = v })
                    |= stringArg "output"

                --
                , P.succeed (\v -> P.Loop { acc | input = v :: acc.input })
                    |= argument

                --
                , P.succeed (P.Done acc)
                    |. P.end
                ]

        boolArg : String -> Parser Bool
        boolArg name =
            P.succeed True
                |. P.symbol ("--" ++ name)
                |. argEnd

        intArg : String -> Parser Int
        intArg name =
            P.succeed identity
                |. P.symbol ("--" ++ name)
                |. P.oneOf
                    [ P.symbol "="
                    , P.symbol "\u{0000}"
                    ]
                |= P.int
                |. argEnd

        stringArg : String -> Parser String
        stringArg name =
            P.succeed identity
                |. P.symbol ("--" ++ name)
                |. P.oneOf
                    [ P.symbol "="
                    , P.symbol "\u{0000}"
                    ]
                |= argument

        argument : Parser String
        argument =
            P.getChompedString
                (P.succeed ()
                    |. P.chompIf (\v -> v /= '-' && v /= '\u{0000}')
                    |. P.chompUntilEndOr "\u{0000}"
                )
                |. argEnd

        argEnd : Parser ()
        argEnd =
            P.oneOf
                [ P.symbol "\u{0000}"
                , P.end
                ]
    in
    P.loop
        { host = "localhost"
        , port_ = 8000

        --
        , root = "."
        , indexAs404 = False
        , open = False

        --
        , sslCert = Nothing
        , sslKey = Nothing

        --
        , elmPath = "elm"
        , debug = False
        , optimize = True
        , input = []
        , output = "elm.js"
        }
        loop
