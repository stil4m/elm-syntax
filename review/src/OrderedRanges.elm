module OrderedRanges exposing (rule)

import Elm.Syntax.Expression as Expression exposing (Expression, RecordSetter)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Writer
import Review.Fix exposing (Fix)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "OrderedRanges" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.Application [ Node _ (Expression.FunctionOrValue [] "Application"), Node _ (Expression.ListExpr ((Node firstItemRange _) :: _)) ] ->
            let
                { start, end } =
                    Node.range node
            in
            if start.row == end.row then
                [ Rule.errorWithFix
                    { message = "Split Application on multiple likes"
                    , details =
                        [ "If like me you have the bad habit of copying nodes from test outputs, this rule will cover for you." ]
                    }
                    (Node.range node)
                    [ Review.Fix.insertAt firstItemRange.start ("\n" ++ String.repeat firstItemRange.start.column " ") ]
                ]

            else
                []

        Expression.RecordExpr recordSetters ->
            case recordSetters of
                [ Node _ ( Node _ "end", _ ), Node _ ( Node _ "start", _ ) ] ->
                    [ Rule.errorWithFix
                        { message = "Writing a node range is more readable from the start"
                        , details =
                            [ "If like me you have the bad habit of copying nodes from test outputs, this rule will cover for you." ]
                        }
                        (Node.range node)
                        [ fixWithFlippedOrder (Node.range node) recordSetters ]
                    ]

                [ Node _ ( Node _ "column", _ ), Node _ ( Node _ "row", _ ) ] ->
                    [ Rule.errorWithFix
                        { message = "Writing a Location is more readable with the row first"
                        , details =
                            [ "If like me you have the bad habit of copying locations from test outputs, this rule will cover for you." ]
                        }
                        (Node.range node)
                        [ fixWithFlippedOrder (Node.range node) recordSetters ]
                    ]

                _ ->
                    []

        _ ->
            []


fixWithFlippedOrder : Range -> List (Node RecordSetter) -> Fix
fixWithFlippedOrder range =
    List.reverse
        >> Expression.RecordExpr
        >> Node.empty
        >> Elm.Writer.writeExpression
        >> Elm.Writer.write
        >> Review.Fix.replaceRangeBy range
