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
                    |= namedArgument "dir"
                    |. Parser.problem "Option --dir is renamed to --root."
                , Parser.succeed (\v -> Parser.Loop { acc | open = v })
                    |= namedArgument "open"
                , Parser.succeed (\v -> Parser.Loop { acc | no404 = v })
                    |= namedArgument "no-404"

                --
                , Parser.succeed (\v -> Parser.Loop { acc | elm = v })
                    |= namedStringArgument "elm"
                , Parser.succeed (\v -> Parser.Loop { acc | debug = v })
                    |= namedArgument "debug"
                , Parser.succeed (\v -> Parser.Loop { acc | optimize = v })
                    |= namedArgument "optimize"
                , Parser.succeed (\v -> Parser.Loop { acc | output = v })
                    |= namedStringArgument "output"

                --
                , Parser.succeed (\v -> Parser.Loop { acc | input = v :: acc.input })
                    |= argument

                --
                , Parser.succeed (Parser.Done acc)
                    |. Parser.end
                ]

        namedArgument : String -> Parser.Parser ()
        namedArgument name =
            Parser.symbol ("--" ++ name)
                |> Parser.andThen (\() -> argumentEnd)

        namedIntArgument : String -> Parser.Parser Int
        namedIntArgument name =
            argumentStart name
                |> Parser.andThen (\() -> Parser.int)
                |> Parser.andThen (\x -> argumentEnd |> Parser.map (\() -> x))

        namedStringArgument : String -> Parser.Parser String
        namedStringArgument name =
            argumentStart name
                |> Parser.andThen (\() -> argument)

        argumentStart : String -> Parser.Parser ()
        argumentStart name =
            Parser.symbol ("--" ++ name)
                |> Parser.andThen
                    (\() ->
                        Parser.oneOf
                            [ Parser.symbol "="
                            , Parser.symbol "\u{0000}"
                            ]
                    )

        argument : Parser.Parser String
        argument =
            Parser.getChompedString
                (Parser.succeed ()
                    |. Parser.chompIf (\v -> v /= '-' && v /= '\u{0000}')
                    |. Parser.chompUntilEndOr "\u{0000}"
                )
                |. argumentEnd

        argumentEnd : Parser.Parser ()
        argumentEnd =
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
