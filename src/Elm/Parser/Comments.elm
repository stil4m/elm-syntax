module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineCommentString, singleLineCommentCore)

import CustomParser exposing (Parser)
import Elm.Parser.Node as Node
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)


singleLineCommentCore : CustomParser.Parser String
singleLineCommentCore =
    CustomParser.symbolFollowedBy "--"
        (CustomParser.chompWhile (\c -> c /= '\u{000D}' && c /= '\n'))
        |> CustomParser.getChompedString


multilineCommentString : CustomParser.Parser String
multilineCommentString =
    CustomParser.offsetSourceAndThen
        (\offset source ->
            case source |> String.slice offset (offset + 3) of
                "{-|" ->
                    problemUnexpectedDocumentation

                _ ->
                    multiLineCommentStringNoCheck
        )


problemUnexpectedDocumentation : Parser a
problemUnexpectedDocumentation =
    CustomParser.problem "unexpected documentation comment"


multiLineCommentStringNoCheck : Parser String
multiLineCommentStringNoCheck =
    CustomParser.nestableMultiComment "{-" "-}"
        |> CustomParser.getChompedString


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
