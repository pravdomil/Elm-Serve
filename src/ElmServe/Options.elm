module ElmServe.Options exposing (..)

import Parser as P exposing ((|.), (|=), Parser)


type alias Options =
    { debug : Maybe Bool
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
        { debug = Nothing
        , optimize = Nothing
        , output = Nothing
        , report = Nothing
        , docs = Nothing
        }
        loop
