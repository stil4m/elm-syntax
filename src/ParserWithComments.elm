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
    ParserFast.Advanced.loop
        listEmptyWithCommentsTuple
        (ParserFast.oneOf2Map
            (\() -> Nothing)
            end
            Just
            p
        )
        (\extension ( commentsSoFar, itemsSoFar ) ->
            case extension of
                Nothing ->
                    ParserFast.Advanced.Done
                        { comments = commentsSoFar
                        , syntax = List.reverse itemsSoFar
                        }

                Just pResult ->
                    ParserFast.Advanced.Loop
                        ( commentsSoFar |> Rope.prependTo pResult.comments
                        , pResult.syntax :: itemsSoFar
                        )
        )


many : Parser (WithComments a) -> Parser (WithComments (List a))
many p =
    ParserFast.Advanced.loop listEmptyWithCommentsTuple
        (ParserFast.orSucceedLazy
            (ParserFast.map Just p)
            (\() -> Nothing)
        )
        (\extension ( commentsSoFar, itemsSoFar ) ->
            case extension of
                Nothing ->
                    ParserFast.Advanced.Done
                        { comments = commentsSoFar
                        , syntax = List.reverse itemsSoFar
                        }

                Just pResult ->
                    ParserFast.Advanced.Loop
                        ( commentsSoFar |> Rope.prependTo pResult.comments
                        , pResult.syntax :: itemsSoFar
                        )
        )


listEmptyWithCommentsTuple : ( Rope a, List b )
listEmptyWithCommentsTuple =
    ( Rope.empty, [] )


{-| Same as [`until`](#until), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
untilWithoutReverse : Parser () -> Parser (WithComments a) -> Parser (WithComments (List a))
untilWithoutReverse end p =
    ParserFast.Advanced.loop
        listEmptyWithCommentsTuple
        (ParserFast.oneOf2Map
            (\() -> Nothing)
            end
            Just
            p
        )
        (\extension ( commentsSoFar, itemsSoFar ) ->
            case extension of
                Nothing ->
                    ParserFast.Advanced.Done
                        { comments = commentsSoFar
                        , syntax = itemsSoFar
                        }

                Just pResult ->
                    ParserFast.Advanced.Loop
                        ( commentsSoFar |> Rope.prependTo pResult.comments
                        , pResult.syntax :: itemsSoFar
                        )
        )


{-| Same as [`many`](#many), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
manyWithoutReverse : Parser (WithComments a) -> Parser (WithComments (List a))
manyWithoutReverse p =
    ParserFast.Advanced.loop listEmptyWithCommentsTuple
        (ParserFast.orSucceedLazy
            (ParserFast.map Just p)
            (\() -> Nothing)
        )
        (\extension ( commentsSoFar, itemsSoFar ) ->
            case extension of
                Nothing ->
                    ParserFast.Advanced.Done
                        { comments = commentsSoFar
                        , syntax = itemsSoFar
                        }

                Just pResult ->
                    ParserFast.Advanced.Loop
                        ( commentsSoFar |> Rope.prependTo pResult.comments
                        , pResult.syntax :: itemsSoFar
                        )
        )


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
