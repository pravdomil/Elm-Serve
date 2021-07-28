module ElmServe.Options exposing (..)

import Parser as P exposing ((|.), (|=), Parser)


type alias Options =
    { debug : Maybe Bool
    , optimize : Maybe Bool
    , output : Maybe String
    , report : Maybe String
    , docs : Maybe String

    --
    , host : Maybe String
    , port_ : Maybe Int

    --
    , root : Maybe String
    , indexAs404 : Maybe Bool
    , open : Maybe Bool

    --
    , ssl : Maybe Bool
    , sslCert : Maybe String
    , sslKey : Maybe String

    --
    , elmPath : Maybe String
    }



--


toString : Options -> String
toString a =
    let
        boolToString : Bool -> String
        boolToString b =
            if b then
                "True"

            else
                "False"
    in
    [ "debug: " ++ (a.debug |> Maybe.map boolToString |> Maybe.withDefault "n/a")
    , "optimize: " ++ (a.optimize |> Maybe.map boolToString |> Maybe.withDefault "n/a")
    , "output: " ++ (a.output |> Maybe.withDefault "n/a")
    , "report: " ++ (a.report |> Maybe.withDefault "n/a")
    , "docs: " ++ (a.docs |> Maybe.withDefault "n/a")
    ]
        |> String.join "\n"



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
                [ P.succeed (\v -> P.Loop { acc | debug = Just v })
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
                , P.succeed (\v -> P.Loop { acc | host = Just v })
                    |= stringArg "host"
                , P.succeed (\v -> P.Loop { acc | port_ = Just v })
                    |= intArg "port"

                --
                , P.succeed (\v -> P.Loop { acc | root = Just v })
                    |= stringArg "root"
                , P.succeed (\v -> P.Loop { acc | indexAs404 = Just v })
                    |= boolArg "index-as-404"
                , P.succeed (\v -> P.Loop { acc | open = Just v })
                    |= boolArg "open"

                --
                , P.succeed (\v -> P.Loop { acc | ssl = Just v })
                    |= boolArg "ssl"
                , P.succeed (\v -> P.Loop { acc | sslCert = Just v })
                    |= stringArg "ssl-cert"
                , P.succeed (\v -> P.Loop { acc | sslKey = Just v })
                    |= stringArg "ssl-key"

                --
                , P.succeed (\v -> P.Loop { acc | elmPath = Just v })
                    |= stringArg "elm-path"

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
                |= P.getChompedString (P.chompUntilEndOr "\u{0000}")
                |. P.oneOf
                    [ P.symbol "\u{0000}"
                    , P.end
                    ]
    in
    P.loop
        (Options
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
        )
        loop
