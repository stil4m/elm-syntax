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
import ParserFast exposing (Parser)
import ParserFast.Advanced
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
                ParserFast.Parser
                    (ParserFast.Advanced.Step
                        ( Comments, List a )
                        (WithComments (List a))
                    )
        step ( commentsSoFar, itemsSoFar ) =
            ParserFast.oneOf2
                (ParserFast.map
                    (\() ->
                        ParserFast.Advanced.Done
                            { comments = commentsSoFar
                            , syntax = List.reverse itemsSoFar
                            }
                    )
                    end
                )
                (ParserFast.map
                    (\pResult ->
                        ParserFast.Advanced.Loop
                            ( commentsSoFar |> Rope.prependTo pResult.comments
                            , pResult.syntax :: itemsSoFar
                            )
                    )
                    p
                )
    in
    ParserFast.Advanced.loop listEmptyWithCommentsTuple step


many : Parser (WithComments a) -> Parser (WithComments (List a))
many p =
    let
        step :
            ( Comments, List a )
            ->
                ParserFast.Parser
                    (ParserFast.Advanced.Step
                        ( Comments, List a )
                        (WithComments (List a))
                    )
        step ( commentsSoFar, itemsSoFar ) =
            ParserFast.orSucceedLazy
                (ParserFast.map
                    (\pResult ->
                        ParserFast.Advanced.Loop
                            ( commentsSoFar |> Rope.prependTo pResult.comments
                            , pResult.syntax :: itemsSoFar
                            )
                    )
                    p
                )
                (\() ->
                    ParserFast.Advanced.Done
                        { comments = commentsSoFar
                        , syntax = List.reverse itemsSoFar
                        }
                )
    in
    ParserFast.Advanced.loop listEmptyWithCommentsTuple step


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
                ParserFast.Parser
                    (ParserFast.Advanced.Step
                        (WithComments (List a))
                        (WithComments (List a))
                    )
        withoutReverseStep soFar =
            ParserFast.oneOf
                [ ParserFast.map (\() -> ParserFast.Advanced.Done soFar) end
                , ParserFast.map
                    (\pResult ->
                        ParserFast.Advanced.Loop
                            { comments = soFar.comments |> Rope.prependTo pResult.comments
                            , syntax = pResult.syntax :: soFar.syntax
                            }
                    )
                    p
                ]
    in
    ParserFast.Advanced.loop listEmptyWithComments withoutReverseStep


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
                ParserFast.Parser
                    (ParserFast.Advanced.Step
                        (WithComments (List a))
                        (WithComments (List a))
                    )
        withoutReverseStep soFar =
            ParserFast.orSucceed
                (ParserFast.map
                    (\pResult ->
                        ParserFast.Advanced.Loop
                            { comments = soFar.comments |> Rope.prependTo pResult.comments
                            , syntax = pResult.syntax :: soFar.syntax
                            }
                    )
                    p
                )
                (ParserFast.Advanced.Done soFar)
    in
    ParserFast.Advanced.loop listEmptyWithComments withoutReverseStep


listEmptyWithComments : WithComments (List b)
listEmptyWithComments =
    { comments = Rope.empty, syntax = [] }


sepBy1 : String -> Parser (WithComments a) -> Parser (WithComments (List a))
sepBy1 sep p =
    ParserFast.map2
        (\head tail ->
            { comments = head.comments |> Rope.prependTo tail.comments
            , syntax = head.syntax :: tail.syntax
            }
        )
        p
        (many (ParserFast.symbolFollowedBy sep p))
