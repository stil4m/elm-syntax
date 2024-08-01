module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineCommentString, singleLineCommentCore)

import Elm.Parser.Node as Node
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing ((|.), (|=), Nestable(..), Parser)
import Parser.Extra


singleLineCommentCore : Parser.Parser (Node String)
singleLineCommentCore =
    Parser.map
        (\( startRow, startColumn ) ->
            \content ->
                \endColumn ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = { row = startRow, column = endColumn }
                        }
                        content
        )
        Parser.getPosition
        |= (Parser.symbol "--"
                |. Parser.chompWhile (\c -> c /= '\u{000D}' && c /= '\n')
                |> Parser.getChompedString
           )
        |= Parser.getCol


multilineCommentString : Parser.Parser String
multilineCommentString =
    Parser.oneOf
        [ Parser.symbol "{-|"
            |> Parser.Extra.continueWith (Parser.problem "unexpected documentation comment")
        , Parser.multiComment "{-" "-}" Nestable
            |> Parser.getChompedString
        ]
        |> Parser.backtrackable


moduleDocumentation : Parser (Node String)
moduleDocumentation =
    declarationDocumentation


declarationDocumentation : Parser.Parser (Node Documentation)
declarationDocumentation =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    -- Here's how a "safe" version would look:
    -- Parser.oneOf
    --[ -- if the next symbol isn't "{-|", we commit to failure
    --  (Parser.symbol "{-" |. Parser.chompIf (\c -> c /= '|'))
    --    |> Parser.backtrackable
    --    |> Parser.Extra.continueWith (Parser.problem "multiline comment should be documentation comment")
    --, Parser.multiComment "{-" "-}" Nestable
    --    |> Parser.getChompedString
    --    |> Node.parserCore
    --]
    --    |> Parser.backtrackable
    Parser.multiComment "{-" "-}" Nestable
        |> Parser.getChompedString
        |> Node.parserCore
