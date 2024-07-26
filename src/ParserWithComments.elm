module ParserWithComments exposing
    ( Comments
    , WithComments
    , many
    , manyWithoutReverse
    , map
    , maybe
    , sepBy
    , sepBy1
    , sepBy1WithState
    )

import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing (Parser)
import Parser.Extra
import Rope exposing (Rope)


type alias WithComments res =
    { comments : Comments, syntax : res }


type alias Comments =
    Rope (Node String)


map : (a -> b) -> Parser (WithComments a) -> Parser (WithComments b)
map f p =
    p
        |> Core.map
            (\commentsAndSyntax ->
                { comments = commentsAndSyntax.comments
                , syntax = f commentsAndSyntax.syntax
                }
            )


keep : Parser (WithComments a) -> Parser (WithComments (a -> b)) -> Parser (WithComments b)
keep rp lp =
    lp
        |> Core.andThen
            (\lpSyntaxAndComments ->
                Core.map
                    (\rpSyntaxAndComments ->
                        { comments =
                            lpSyntaxAndComments.comments
                                |> Rope.prependTo rpSyntaxAndComments.comments
                        , syntax = lpSyntaxAndComments.syntax rpSyntaxAndComments.syntax
                        }
                    )
                    rp
            )


maybe : Parser (WithComments a) -> Parser (WithComments (Maybe a))
maybe p =
    Core.oneOf
        [ p
            |> Core.map
                (\syntaxAndComments ->
                    { comments = syntaxAndComments.comments
                    , syntax = Just syntaxAndComments.syntax
                    }
                )
        , Core.succeed { comments = Rope.empty, syntax = Nothing }
        ]


maybeMap : (a -> b) -> b -> Parser (WithComments a) -> Parser (WithComments b)
maybeMap onJust onNothing p =
    Core.oneOf
        [ p
            |> Core.map
                (\syntaxAndComments ->
                    { comments = syntaxAndComments.comments
                    , syntax = onJust syntaxAndComments.syntax
                    }
                )
        , Core.succeed { comments = Rope.empty, syntax = onNothing }
        ]


many : Parser (WithComments a) -> Parser (WithComments (List a))
many p =
    let
        manyWithoutReverseStep :
            ( List Comments, List a )
            ->
                Core.Parser
                    (Core.Step
                        ( List Comments, List a )
                        { comments : Comments, syntax : List a }
                    )
        manyWithoutReverseStep ( commentsSoFar, items ) =
            Core.oneOf
                [ p
                    |> Core.map
                        (\pResult ->
                            Core.Loop
                                ( pResult.comments :: commentsSoFar
                                , pResult.syntax :: items
                                )
                        )
                , Core.lazy
                    (\() ->
                        Core.succeed
                            (Core.Done
                                { comments = Rope.flatFromList (List.reverse commentsSoFar)
                                , syntax = List.reverse items
                                }
                            )
                    )
                ]
    in
    Core.loop ( [], [] ) manyWithoutReverseStep


{-| Same as [`many`](#many), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
manyWithoutReverse : Parser (WithComments a) -> Parser (WithComments (List a))
manyWithoutReverse p =
    let
        manyWithoutReverseStep :
            ( List Comments, List a )
            ->
                Core.Parser
                    (Core.Step
                        ( List Comments, List a )
                        { comments : Comments, syntax : List a }
                    )
        manyWithoutReverseStep ( commentsSoFar, items ) =
            Core.oneOf
                [ p
                    |> Core.map
                        (\pResult ->
                            Core.Loop
                                ( pResult.comments :: commentsSoFar
                                , pResult.syntax :: items
                                )
                        )
                , Core.lazy
                    (\() ->
                        Core.succeed
                            (Core.Done
                                { comments = Rope.flatFromList (List.reverse commentsSoFar)
                                , syntax = items
                                }
                            )
                    )
                ]
    in
    Core.loop ( [], [] ) manyWithoutReverseStep


sepBy : String -> Parser (WithComments a) -> Parser (WithComments (List a))
sepBy sep p =
    maybeMap identity
        []
        (sepBy1 sep p)


sepBy1 : String -> Parser (WithComments a) -> Parser (WithComments (List a))
sepBy1 sep p =
    map cons p
        |> keep (many (Core.symbol sep |> Parser.Extra.continueWith p))


sepBy1WithState : Parser Comments -> Parser (WithComments a) -> Parser (WithComments (List a))
sepBy1WithState sep p =
    map cons p
        |> keep
            (many
                (sep
                    |> Core.andThen
                        (\onlyComments ->
                            p
                                |> Core.map
                                    (\pResult ->
                                        { comments = onlyComments |> Rope.prependTo pResult.comments
                                        , syntax = pResult.syntax
                                        }
                                    )
                        )
                )
            )


cons : a -> List a -> List a
cons first =
    \rest -> first :: rest
