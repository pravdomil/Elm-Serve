module ElmServe.Options exposing (..)

import Parser as P exposing ((|.), (|=), Parser)


type alias Options =
    { host : String
    , port_ : Int

    --
    , root : Maybe String
    , indexAs404 : Maybe Bool
    , open : Bool

    --
    , sslCert : Maybe String
    , sslKey : Maybe String

    --
    , elmPath : Maybe String
    , debug : Maybe Bool
    , optimize : Maybe Bool
    , output : Maybe String
    , report : Maybe String
    , docs : Maybe String
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
    , "Root:         " ++ (a.root |> Maybe.withDefault "-")
    , "Index As 404: " ++ (a.indexAs404 |> Maybe.map boolToString |> Maybe.withDefault "-")
    , "Open:         " ++ (a.open |> boolToString)
    , "SSL Cert:     " ++ (a.sslCert |> Maybe.withDefault "-")
    , "SSL Key:      " ++ (a.sslKey |> Maybe.withDefault "-")
    , "Elm Path:     " ++ (a.elmPath |> Maybe.withDefault "-")
    , "Debug:        " ++ (a.debug |> Maybe.map boolToString |> Maybe.withDefault "-")
    , "Optimize:     " ++ (a.optimize |> Maybe.map boolToString |> Maybe.withDefault "-")
    , "Output:       " ++ (a.output |> Maybe.withDefault "-")
    , "Report:       " ++ (a.report |> Maybe.withDefault "-")
    , "Docs:         " ++ (a.docs |> Maybe.withDefault "-")
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
                , P.succeed (\v -> P.Loop { acc | root = Just v })
                    |= stringArg "root"
                , P.succeed (\v -> P.Loop { acc | indexAs404 = Just v })
                    |= boolArg "index-as-404"
                , P.succeed (\v -> P.Loop { acc | open = v })
                    |= boolArg "open"

                --
                , P.succeed (\v -> P.Loop { acc | sslCert = Just v })
                    |= stringArg "ssl-cert"
                , P.succeed (\v -> P.Loop { acc | sslKey = Just v })
                    |= stringArg "ssl-key"

                --
                , P.succeed (\v -> P.Loop { acc | elmPath = Just v })
                    |= stringArg "elm-path"
                , P.succeed (\v -> P.Loop { acc | debug = Just v })
                    |= boolArg "debug"
                , P.succeed (\v -> P.Loop { acc | optimize = Just v })
                    |= boolArg "optimize"
                , P.succeed (\v -> P.Loop { acc | output = Just v })
                    |= stringArg "output"
                , P.succeed (\v -> P.Loop { acc | report = Just v })
                    |= stringArg "report"
                , P.succeed (\v -> P.Loop { acc | docs = Just v })
                    |= stringArg "docs"

                --
                , P.succeed (P.Done acc)
                    |. P.end
                ]

        boolArg : String -> Parser Bool
        boolArg name =
            P.succeed True
                |. P.symbol ("--" ++ name)
                |. P.oneOf
                    [ P.symbol "\u{0000}"
                    , P.end
                    ]

        intArg : String -> Parser Int
        intArg name =
            P.succeed identity
                |. P.symbol ("--" ++ name)
                |. P.oneOf
                    [ P.symbol "="
                    , P.symbol "\u{0000}"
                    ]
                |= P.int
                |. P.oneOf
                    [ P.symbol "\u{0000}"
                    , P.end
                    ]

        stringArg : String -> Parser String
        stringArg name =
            P.succeed identity
                |. P.symbol ("--" ++ name)
                |. P.oneOf
                    [ P.symbol "="
                    , P.symbol "\u{0000}"
                    ]
                |= P.getChompedString
                    (P.succeed identity
                        |= P.chompIf ((/=) '\u{0000}')
                        |. P.chompUntilEndOr "\u{0000}"
                    )
                |. P.oneOf
                    [ P.symbol "\u{0000}"
                    , P.end
                    ]
    in
    P.loop
        { debug = Nothing
        , optimize = Nothing
        , output = Nothing
        , report = Nothing
        , docs = Nothing

        --
        , host = "localhost"
        , port_ = 8000

        --
        , root = Nothing
        , indexAs404 = Nothing
        , open = False

        --
        , sslCert = Nothing
        , sslKey = Nothing

        --
        , elmPath = Nothing
        }
        loop
