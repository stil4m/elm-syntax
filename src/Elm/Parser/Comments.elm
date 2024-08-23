module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineComment, singleLineComment)

import Elm.Parser.Node as Node
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)
import ParserFast exposing (Parser)


singleLineComment : ParserFast.Parser (Node String)
singleLineComment =
    ParserFast.symbolFollowedBy "--"
        (ParserFast.whileMap
            (\c -> c /= '\u{000D}' && c /= '\n')
            (\content -> "--" ++ content)
        )
        |> Node.parserCore


multilineComment : ParserFast.Parser (Node String)
multilineComment =
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case source |> String.slice offset (offset + 3) of
                "{-|" ->
                    problemUnexpectedDocumentation

                _ ->
                    multiLineCommentNoCheck
        )


problemUnexpectedDocumentation : Parser a
problemUnexpectedDocumentation =
    ParserFast.problem "unexpected documentation comment"


multiLineCommentNoCheck : Parser (Node String)
multiLineCommentNoCheck =
    ParserFast.nestableMultiComment ( '{', "-" ) ( '-', "}" )
        |> Node.parserCore


moduleDocumentation : Parser (Node String)
moduleDocumentation =
    declarationDocumentation


declarationDocumentation : ParserFast.Parser (Node Documentation)
declarationDocumentation =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    ParserFast.nestableMultiComment ( '{', "-" ) ( '-', "}" )
        |> Node.parserCore
