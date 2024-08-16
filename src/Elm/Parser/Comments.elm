module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineCommentString, singleLineCommentCore)

import Elm.Parser.Node as Node
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)
import ParserFast exposing (Parser)


singleLineCommentCore : ParserFast.Parser String
singleLineCommentCore =
    ParserFast.symbolFollowedBy "--"
        (ParserFast.chompWhile (\c -> c /= '\u{000D}' && c /= '\n'))
        |> ParserFast.getChompedString


multilineCommentString : ParserFast.Parser String
multilineCommentString =
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case source |> String.slice offset (offset + 3) of
                "{-|" ->
                    problemUnexpectedDocumentation

                _ ->
                    multiLineCommentStringNoCheck
        )


problemUnexpectedDocumentation : Parser a
problemUnexpectedDocumentation =
    ParserFast.problem "unexpected documentation comment"


multiLineCommentStringNoCheck : Parser String
multiLineCommentStringNoCheck =
    ParserFast.nestableMultiComment "{-" "-}"
        |> ParserFast.getChompedString


moduleDocumentation : Parser (Node String)
moduleDocumentation =
    declarationDocumentation


declarationDocumentation : ParserFast.Parser (Node Documentation)
declarationDocumentation =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    ParserFast.nestableMultiComment "{-" "-}"
        |> ParserFast.getChompedString
        |> Node.parserCore
