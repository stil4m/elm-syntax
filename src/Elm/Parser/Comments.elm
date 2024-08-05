module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineCommentString, singleLineCommentCore)

import CustomParser exposing (Parser)
import CustomParser.Extra
import Elm.Parser.Node as Node
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)


singleLineCommentCore : CustomParser.Parser String
singleLineCommentCore =
    CustomParser.symbol "--" ()
        |> CustomParser.ignore (CustomParser.chompWhile (\c -> c /= '\u{000D}' && c /= '\n'))
        |> CustomParser.getChompedString


multilineCommentString : CustomParser.Parser String
multilineCommentString =
    CustomParser.oneOf
        [ CustomParser.symbol "{-|" ()
            |> CustomParser.Extra.continueWith (CustomParser.problem "unexpected documentation comment")
        , CustomParser.nestableMultiComment "{-" "-}"
            |> CustomParser.getChompedString
        ]
        |> CustomParser.backtrackable


moduleDocumentation : Parser (Node String)
moduleDocumentation =
    declarationDocumentation


declarationDocumentation : CustomParser.Parser (Node Documentation)
declarationDocumentation =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    -- Here's how a "safe" version would look:
    -- CustomParser.oneOf
    --[ -- if the next symbol isn't "{-|", we commit to failure
    --  (CustomParser.symbol "{-" |> CustomParser.ignore CustomParser.chompIf (\c -> c /= '|'))
    --    |> CustomParser.backtrackable
    --    |> CustomParser.Extra.continueWith (CustomParser.problem "multiline comment should be documentation comment")
    --, CustomParser.multiComment "{-" "-}" Nestable
    --    |> CustomParser.getChompedString
    --    |> Node.parserCore
    --]
    --    |> CustomParser.backtrackable
    CustomParser.nestableMultiComment "{-" "-}"
        |> CustomParser.getChompedString
        |> Node.parserCore
