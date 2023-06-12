module ElmServe.Options exposing (..)

import HttpServer
import Parser


type alias Options =
    { http : HttpServer.Options

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


parse : List String -> Result (List Parser.DeadEnd) Options
parse a =
    Parser.run parser (String.join "\u{0000}" a)


parser : Parser.Parser Options
parser =
    let
        loop : Options -> Parser.Parser (Parser.Step Options Options)
        loop acc =
            Parser.oneOf
                [ Parser.succeed (\v -> Parser.Loop { acc | host = v })
                    |= stringArg "host"
                , Parser.succeed (\v -> Parser.Loop { acc | port_ = v })
                    |= intArg "port"
                , Parser.succeed (\v1 v2 -> Parser.Loop { acc | ssl = Just { cert = v1, key = v2 } })
                    |= stringArg "ssl"
                    |= argument

                --
                , Parser.succeed (\v -> Parser.Loop { acc | root = v })
                    |= stringArg "root"
                , Parser.succeed (\_ -> Parser.Loop acc)
                    |= boolArg "dir"
                    |. Parser.problem "Option --dir is renamed to --root."
                , Parser.succeed (\v -> Parser.Loop { acc | open = v })
                    |= boolArg "open"
                , Parser.succeed (\v -> Parser.Loop { acc | no404 = v })
                    |= boolArg "no-404"

                --
                , Parser.succeed (\v -> Parser.Loop { acc | elm = v })
                    |= stringArg "elm"
                , Parser.succeed (\v -> Parser.Loop { acc | debug = v })
                    |= boolArg "debug"
                , Parser.succeed (\v -> Parser.Loop { acc | optimize = v })
                    |= boolArg "optimize"
                , Parser.succeed (\v -> Parser.Loop { acc | output = v })
                    |= stringArg "output"

                --
                , Parser.succeed (\v -> Parser.Loop { acc | input = v :: acc.input })
                    |= argument

                --
                , Parser.succeed (Parser.Done acc)
                    |. Parser.end
                ]

        boolArg : String -> Parser.Parser Bool
        boolArg name =
            Parser.succeed True
                |. Parser.symbol ("--" ++ name)
                |. argEnd

        intArg : String -> Parser.Parser Int
        intArg name =
            Parser.succeed identity
                |. Parser.symbol ("--" ++ name)
                |. Parser.oneOf
                    [ Parser.symbol "="
                    , Parser.symbol "\u{0000}"
                    ]
                |= Parser.int
                |. argEnd

        stringArg : String -> Parser.Parser String
        stringArg name =
            Parser.succeed identity
                |. Parser.symbol ("--" ++ name)
                |. Parser.oneOf
                    [ Parser.symbol "="
                    , Parser.symbol "\u{0000}"
                    ]
                |= argument

        argument : Parser.Parser String
        argument =
            Parser.getChompedString
                (Parser.succeed ()
                    |. Parser.chompIf (\v -> v /= '-' && v /= '\u{0000}')
                    |. Parser.chompUntilEndOr "\u{0000}"
                )
                |. argEnd

        argEnd : Parser.Parser ()
        argEnd =
            Parser.oneOf
                [ Parser.symbol "\u{0000}"
                , Parser.end
                ]
    in
    Parser.loop
        { server = HttpServer.Options "localhost" 8000 Nothing

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
