module AstFormatting exposing (rule)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Fix
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "AstFormatting" ()
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
                    { message = "Split Application on multiple lines"
                    , details =
                        [ "If like me you have the bad habit of copying nodes from test outputs, this rule will cover for you." ]
                    }
                    (Node.range node)
                    [ Review.Fix.insertAt firstItemRange.start ("\n" ++ String.repeat firstItemRange.start.column " ") ]
                ]

            else
                []

        _ ->
            []
