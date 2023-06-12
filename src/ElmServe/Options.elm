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
                    |= namedStringArgument "host"
                , Parser.succeed (\v -> Parser.Loop { acc | port_ = v })
                    |= namedIntArgument "port"
                , Parser.succeed (\v1 v2 -> Parser.Loop { acc | ssl = Just { cert = v1, key = v2 } })
                    |= namedStringArgument "ssl"
                    |= argument

                --
                , Parser.succeed (\v -> Parser.Loop { acc | root = v })
                    |= namedStringArgument "root"
                , Parser.succeed (\_ -> Parser.Loop acc)
                    |= flag "dir"
                    |. Parser.problem "Option --dir is renamed to --root."
                , Parser.succeed (\v -> Parser.Loop { acc | open = v })
                    |= flag "open"
                , Parser.succeed (\v -> Parser.Loop { acc | no404 = v })
                    |= flag "no-404"

                --
                , Parser.succeed (\v -> Parser.Loop { acc | elm = v })
                    |= namedStringArgument "elm"
                , Parser.succeed (\v -> Parser.Loop { acc | debug = v })
                    |= flag "debug"
                , Parser.succeed (\v -> Parser.Loop { acc | optimize = v })
                    |= flag "optimize"
                , Parser.succeed (\v -> Parser.Loop { acc | output = v })
                    |= namedStringArgument "output"

                --
                , Parser.succeed (\v -> Parser.Loop { acc | input = v :: acc.input })
                    |= argument

                --
                , Parser.succeed (Parser.Done acc)
                    |. Parser.end
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



--


argument : Parser.Parser String
argument =
    Parser.getChompedString
        (Parser.chompIf (\x -> x /= '\u{0000}')
            |> Parser.andThen (\() -> Parser.chompUntilEndOr "\u{0000}")
        )
        |> Parser.andThen (\x -> argumentEnd |> Parser.map (\() -> x))


namedArgument : String -> Parser.Parser a -> Parser.Parser a
namedArgument name a =
    Parser.symbol ("--" ++ name)
        |> Parser.andThen
            (\() ->
                Parser.oneOf
                    [ Parser.symbol "="
                    , Parser.symbol "\u{0000}"
                    ]
            )
        |> Parser.andThen (\() -> a)
        |> Parser.andThen (\x -> argumentEnd |> Parser.map (\() -> x))


flag : String -> Parser.Parser a -> Parser.Parser a
flag name a =
    Parser.symbol ("--" ++ name)
        |> Parser.andThen
            (\() ->
                Parser.oneOf
                    [ Parser.symbol "="
                    , Parser.symbol "\u{0000}"
                    ]
            )
        |> Parser.andThen (\() -> a)
        |> Parser.andThen (\x -> argumentEnd |> Parser.map (\() -> x))


argumentEnd : Parser.Parser ()
argumentEnd =
    Parser.oneOf
        [ Parser.symbol "\u{0000}"
        , Parser.end
        ]
