module ParserWithComments exposing
    ( Comments
    , WithComments
    , many
    , manyWithoutReverse
    , sepBy1
    , until
    , untilWithoutReverse
    )

import Elm.Syntax.Node exposing (Node)
import Parser exposing ((|=), Parser)
import Parser.Extra
import Rope exposing (Rope)


type alias WithComments res =
    { comments : Comments, syntax : res }


type alias Comments =
    Rope (Node String)


until : Parser () -> Parser (WithComments a) -> Parser (WithComments (List a))
until end p =
    let
        step :
            ( Comments, List a )
            ->
                Parser.Parser
                    (Parser.Step
                        ( Comments, List a )
                        (WithComments (List a))
                    )
        step ( commentsSoFar, itemsSoFar ) =
            Parser.oneOf
                [ Parser.map
                    (\() ->
                        Parser.Done
                            { comments = commentsSoFar
                            , syntax = List.reverse itemsSoFar
                            }
                    )
                    end
                , Parser.map
                    (\pResult ->
                        Parser.Loop
                            ( commentsSoFar |> Rope.prependTo pResult.comments
                            , pResult.syntax :: itemsSoFar
                            )
                    )
                    p
                ]
    in
    Parser.loop listEmptyWithCommentsTuple step


many : Parser (WithComments a) -> Parser (WithComments (List a))
many p =
    let
        step :
            ( Comments, List a )
            ->
                Parser.Parser
                    (Parser.Step
                        ( Comments, List a )
                        (WithComments (List a))
                    )
        step ( commentsSoFar, itemsSoFar ) =
            Parser.oneOf
                [ Parser.map
                    (\pResult ->
                        Parser.Loop
                            ( commentsSoFar |> Rope.prependTo pResult.comments
                            , pResult.syntax :: itemsSoFar
                            )
                    )
                    p
                , Parser.lazy
                    (\() ->
                        Parser.succeed
                            (Parser.Done
                                { comments = commentsSoFar
                                , syntax = List.reverse itemsSoFar
                                }
                            )
                    )
                ]
    in
    Parser.loop listEmptyWithCommentsTuple step


listEmptyWithCommentsTuple : ( Rope a, List b )
listEmptyWithCommentsTuple =
    ( Rope.empty, [] )


{-| Same as [`until`](#until), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
untilWithoutReverse : Parser () -> Parser (WithComments a) -> Parser (WithComments (List a))
untilWithoutReverse end p =
    let
        withoutReverseStep :
            WithComments (List a)
            ->
                Parser.Parser
                    (Parser.Step
                        (WithComments (List a))
                        (WithComments (List a))
                    )
        withoutReverseStep soFar =
            Parser.oneOf
                [ Parser.map (\() -> Parser.Done soFar) end
                , Parser.map
                    (\pResult ->
                        Parser.Loop
                            { comments = soFar.comments |> Rope.prependTo pResult.comments
                            , syntax = pResult.syntax :: soFar.syntax
                            }
                    )
                    p
                ]
    in
    Parser.loop listEmptyWithComments withoutReverseStep


{-| Same as [`many`](#many), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
manyWithoutReverse : Parser (WithComments a) -> Parser (WithComments (List a))
manyWithoutReverse p =
    let
        withoutReverseStep :
            WithComments (List a)
            ->
                Parser.Parser
                    (Parser.Step
                        (WithComments (List a))
                        (WithComments (List a))
                    )
        withoutReverseStep soFar =
            Parser.oneOf
                [ Parser.map
                    (\pResult ->
                        Parser.Loop
                            { comments = soFar.comments |> Rope.prependTo pResult.comments
                            , syntax = pResult.syntax :: soFar.syntax
                            }
                    )
                    p
                , Parser.succeed (Parser.Done soFar)
                ]
    in
    Parser.loop listEmptyWithComments withoutReverseStep


listEmptyWithComments : WithComments (List b)
listEmptyWithComments =
    { comments = Rope.empty, syntax = [] }


sepBy1 : String -> Parser (WithComments a) -> Parser (WithComments (List a))
sepBy1 sep p =
    Parser.map
        (\head ->
            \tail ->
                { comments = head.comments |> Rope.prependTo tail.comments
                , syntax = head.syntax :: tail.syntax
                }
        )
        p
        |= many (Parser.symbol sep |> Parser.Extra.continueWith p)
