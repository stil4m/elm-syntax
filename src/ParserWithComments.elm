module ParserWithComments exposing
    ( ParserWithComments
    , andThen
    , between
    , continueWith
    , continueWithCore
    , flattenFromCore
    , fromCore
    , fromCoreIgnore
    , fromCoreKeep
    , fromCoreMap
    , ignore
    , keep
    , keepFromCore
    , loop
    , many
    , manyWithoutReverse
    , map
    , maybe
    , maybeIgnore
    , maybeMap
    , sepBy
    , sepBy1
    , sepBy1WithState
    , succeed
    )

import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|.))
import Parser.Extra
import Rope exposing (Rope)


type alias ParserWithComments res =
    Core.Parser { comments : Rope (Node String), syntax : res }


fromCoreMap : (res -> changedRes) -> Core.Parser res -> ParserWithComments changedRes
fromCoreMap resChange p =
    Core.map (\v -> { comments = Rope.empty, syntax = resChange v }) p


fromCore : Core.Parser res -> ParserWithComments res
fromCore p =
    Core.map (\v -> { comments = Rope.empty, syntax = v }) p


map : (a -> b) -> ParserWithComments a -> ParserWithComments b
map f p =
    p
        |> Core.map
            (\commentsAndSyntax ->
                { comments = commentsAndSyntax.comments
                , syntax = f commentsAndSyntax.syntax
                }
            )


andThen : (a -> ParserWithComments b) -> ParserWithComments a -> ParserWithComments b
andThen f p =
    p
        |> Core.andThen
            (\pSyntaxAndComments ->
                f pSyntaxAndComments.syntax
                    |> Core.map
                        (\afterAndThen ->
                            { comments =
                                pSyntaxAndComments.comments
                                    |> Rope.prependTo afterAndThen.comments
                            , syntax = afterAndThen.syntax
                            }
                        )
            )


continueWith : ParserWithComments b -> ParserWithComments () -> ParserWithComments b
continueWith next p =
    p
        |> Core.andThen
            (\pOnlyComments ->
                next
                    |> Core.map
                        (\nextResult ->
                            { comments =
                                pOnlyComments.comments
                                    |> Rope.prependTo nextResult.comments
                            , syntax = nextResult.syntax
                            }
                        )
            )


fromCoreIgnore : ParserWithComments () -> Core.Parser a -> ParserWithComments a
fromCoreIgnore next p =
    p
        |> Core.andThen
            (\pResult ->
                next
                    |> Core.map
                        (\nextOnlyComments ->
                            { comments = nextOnlyComments.comments
                            , syntax = pResult
                            }
                        )
            )


{-| Equivalent to andThen fromCore (but a tiny bit faster)
-}
flattenFromCore : ParserWithComments (Core.Parser b) -> ParserWithComments b
flattenFromCore p =
    p
        |> Core.andThen
            (\resultCoreParserAndComments ->
                resultCoreParserAndComments.syntax
                    |> Core.map
                        (\result ->
                            { comments = resultCoreParserAndComments.comments
                            , syntax = result
                            }
                        )
            )


keep : ParserWithComments a -> ParserWithComments (a -> b) -> ParserWithComments b
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


fromCoreKeep : ParserWithComments a -> Core.Parser (a -> b) -> ParserWithComments b
fromCoreKeep rp lp =
    lp
        |> Core.andThen
            (\lpSyntax ->
                Core.map
                    (\rpSyntaxAndComments ->
                        { comments = rpSyntaxAndComments.comments
                        , syntax = lpSyntax rpSyntaxAndComments.syntax
                        }
                    )
                    rp
            )


keepFromCore : Core.Parser a -> ParserWithComments (a -> b) -> ParserWithComments b
keepFromCore rp lp =
    lp
        |> Core.andThen
            (\lpCommentsAndSyntax ->
                Core.map
                    (\rpSyntax ->
                        { comments = lpCommentsAndSyntax.comments
                        , syntax = lpCommentsAndSyntax.syntax rpSyntax
                        }
                    )
                    rp
            )


succeed : a -> ParserWithComments a
succeed res =
    Core.succeed { comments = Rope.empty, syntax = res }


maybe : ParserWithComments a -> ParserWithComments (Maybe a)
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


maybeMap : (a -> b) -> b -> ParserWithComments a -> ParserWithComments b
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


maybeIgnore : ParserWithComments () -> ParserWithComments ()
maybeIgnore p =
    Core.oneOf
        [ p
        , Core.succeed { comments = Rope.empty, syntax = () }
        ]


many : ParserWithComments a -> ParserWithComments (List a)
many p =
    let
        manyWithoutReverseStep :
            ( List (Rope (Node String)), List a )
            ->
                Core.Parser
                    (Core.Step
                        ( List (Rope (Node String)), List a )
                        { comments : Rope (Node String), syntax : List a }
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
-}
manyWithoutReverse : ParserWithComments a -> ParserWithComments (List a)
manyWithoutReverse p =
    let
        manyWithoutReverseStep :
            ( List (Rope (Node String)), List a )
            ->
                Core.Parser
                    (Core.Step
                        ( List (Rope (Node String)), List a )
                        { comments : Rope (Node String), syntax : List a }
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


{-| Same as [`many`](#many), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.
-}
loop : a -> (a -> ParserWithComments (Core.Step a b)) -> ParserWithComments b
loop initialState step =
    let
        stepWithComments :
            ( List (Rope (Node String)), a )
            ->
                Core.Parser
                    (Core.Step
                        ( List (Rope (Node String)), a )
                        { comments : Rope (Node String), syntax : b }
                    )
        stepWithComments ( commentsSoFar, state ) =
            step state
                |> Core.map
                    (\pResult ->
                        case pResult.syntax of
                            Core.Loop pResultLoop ->
                                Core.Loop
                                    ( pResult.comments :: commentsSoFar
                                    , pResultLoop
                                    )

                            Core.Done pResultDone ->
                                Core.Done
                                    { comments = Rope.flatFromList (List.reverse (pResult.comments :: commentsSoFar))
                                    , syntax = pResultDone
                                    }
                    )
    in
    Core.loop ( [], initialState ) stepWithComments


sepBy : String -> ParserWithComments a -> ParserWithComments (List a)
sepBy sep p =
    maybeMap identity
        []
        (sepBy1 sep p)


sepBy1 : String -> ParserWithComments a -> ParserWithComments (List a)
sepBy1 sep p =
    map cons p
        |> keep (many (Core.symbol sep |> Parser.Extra.continueWith p))


sepBy1WithState : ParserWithComments () -> ParserWithComments a -> ParserWithComments (List a)
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
                                        { comments = onlyComments.comments |> Rope.prependTo pResult.comments
                                        , syntax = pResult.syntax
                                        }
                                    )
                        )
                )
            )


between : Core.Parser () -> Core.Parser () -> ParserWithComments a -> ParserWithComments a
between lp rp p =
    (lp
        |> Parser.Extra.continueWith p
    )
        |. rp


ignore : ParserWithComments () -> ParserWithComments a -> ParserWithComments a
ignore dropped target =
    target
        |> Core.andThen
            (\targetResult ->
                dropped
                    |> Core.map
                        (\droppedOnlyComments ->
                            { comments =
                                targetResult.comments
                                    |> Rope.prependTo droppedOnlyComments.comments
                            , syntax = targetResult.syntax
                            }
                        )
            )


continueWithCore : Core.Parser a -> ParserWithComments () -> ParserWithComments a
continueWithCore target dropped =
    dropped
        |> Core.andThen
            (\onlyComments ->
                target
                    |> Core.map
                        (\a ->
                            { comments = onlyComments.comments
                            , syntax = a
                            }
                        )
            )


cons : a -> List a -> List a
cons first =
    \rest -> first :: rest
