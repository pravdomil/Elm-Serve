module ElmServe.Options exposing (..)

import Parser as P exposing ((|.), (|=))


type alias Options =
    { host : String
    , port_ : Int
    , ssl : Maybe { cert : String, key : String }

    --
    , root : String
    , open : Bool
    , no404 : Bool

    --
    , elm : String
    , debug : Bool
    , optimize : Bool
    , input : List String
    , output : String
    }



--


parse : List String -> Result (List P.DeadEnd) Options
parse a =
    String.join "\u{0000}" a
        |> P.run parser


parser : P.Parser Options
parser =
    let
        loop : Options -> P.Parser (P.Step Options Options)
        loop acc =
            P.oneOf
                [ P.succeed (\v -> P.Loop { acc | host = v })
                    |= stringArg "host"
                , P.succeed (\v -> P.Loop { acc | port_ = v })
                    |= intArg "port"
                , P.succeed (\v1 v2 -> P.Loop { acc | ssl = Just { cert = v1, key = v2 } })
                    |= stringArg "ssl"
                    |= argument

                --
                , P.succeed (\v -> P.Loop { acc | root = v })
                    |= stringArg "root"
                , P.succeed (\_ -> P.Loop acc)
                    |= boolArg "dir"
                    |. P.problem "Option --dir is renamed to --root."
                , P.succeed (\v -> P.Loop { acc | open = v })
                    |= boolArg "open"
                , P.succeed (\v -> P.Loop { acc | no404 = v })
                    |= boolArg "no-404"

                --
                , P.succeed (\v -> P.Loop { acc | elm = v })
                    |= stringArg "elm"
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

        boolArg : String -> P.Parser Bool
        boolArg name =
            P.succeed True
                |. P.symbol ("--" ++ name)
                |. argEnd

        intArg : String -> P.Parser Int
        intArg name =
            P.succeed identity
                |. P.symbol ("--" ++ name)
                |. P.oneOf
                    [ P.symbol "="
                    , P.symbol "\u{0000}"
                    ]
                |= P.int
                |. argEnd

        stringArg : String -> P.Parser String
        stringArg name =
            P.succeed identity
                |. P.symbol ("--" ++ name)
                |. P.oneOf
                    [ P.symbol "="
                    , P.symbol "\u{0000}"
                    ]
                |= argument

        argument : P.Parser String
        argument =
            P.getChompedString
                (P.succeed ()
                    |. P.chompIf (\v -> v /= '-' && v /= '\u{0000}')
                    |. P.chompUntilEndOr "\u{0000}"
                )
                |. argEnd

        argEnd : P.Parser ()
        argEnd =
            P.oneOf
                [ P.symbol "\u{0000}"
                , P.end
                ]
    in
    P.loop
        { host = "localhost"
        , port_ = 8000
        , ssl = Nothing

        --
        , root = "."
        , open = False
        , no404 = False

        --
        , elm = "elm"
        , debug = False
        , optimize = False
        , input = []
        , output = "elm.js"
        }
        loop
