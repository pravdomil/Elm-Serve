module ElmServe.Options exposing (..)

import HttpServer
import Parser exposing ((|=))


type alias Options =
    { server : HttpServer.Options

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
        map2 fn a b =
            Parser.succeed fn
                |= a
                |= b

        loop : Options -> Parser.Parser (Parser.Step Options Options)
        loop acc =
            Parser.oneOf
                [ namedArgument "host" stringArgument
                    |> Parser.map (\x -> Parser.Loop { acc | server = (\x2 -> { x2 | host = x }) acc.server })
                , namedArgument "port" Parser.int
                    |> Parser.map (\x -> Parser.Loop { acc | server = (\x2 -> { x2 | port_ = x }) acc.server })
                , namedArgument "ssl" (map2 (\x x2 -> { cert = x, key = x2 }) stringArgument stringArgument)
                    |> Parser.map (\x -> Parser.Loop { acc | server = (\x2 -> { x2 | ssl = Just x }) acc.server })

                --
                , namedArgument "root" stringArgument
                    |> Parser.map (\x -> Parser.Loop { acc | root = x })
                , flag "dir"
                    |> Parser.andThen (\() -> Parser.problem "Option --dir is renamed to --root.")
                , flag "open"
                    |> Parser.map (\() -> Parser.Loop { acc | open = True })
                , flag "no-404"
                    |> Parser.map (\() -> Parser.Loop { acc | no404 = True })

                --
                , namedArgument "elm" stringArgument
                    |> Parser.map (\x -> Parser.Loop { acc | elm = x })
                , flag "debug"
                    |> Parser.map (\() -> Parser.Loop { acc | debug = True })
                , flag "optimize"
                    |> Parser.map (\() -> Parser.Loop { acc | optimize = True })
                , namedArgument "output" stringArgument
                    |> Parser.map (\x -> Parser.Loop { acc | output = x })

                --
                , stringArgument
                    |> Parser.map (\x -> Parser.Loop { acc | input = x :: acc.input })

                --
                , Parser.end
                    |> Parser.map (\() -> Parser.Done acc)
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


stringArgument : Parser.Parser String
stringArgument =
    Parser.getChompedString
        (Parser.chompIf (\x -> x /= '\u{0000}')
            |> Parser.andThen (\() -> Parser.chompUntilEndOr "\u{0000}")
        )
        |> Parser.andThen (\x -> argumentEnd |> Parser.map (\() -> x))


flag : String -> Parser.Parser ()
flag name =
    Parser.symbol ("--" ++ name)
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


argumentEnd : Parser.Parser ()
argumentEnd =
    Parser.oneOf
        [ Parser.symbol "\u{0000}"
        , Parser.end
        ]
