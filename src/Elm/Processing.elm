module Elm.Processing exposing (process)

{-| Processing raw files with the context of other files and dependencies.

@docs process

-}

import Dict
import Elm.Internal.RawFile as InternalRawFile
import Elm.Operators exposing (SimpleInfix)
import Elm.RawFile as RawFile
import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import List.Extra


{-| Process a rawfile with a context.
Operator precedence and documentation will be fixed.
-}
process : RawFile.RawFile -> File
process (InternalRawFile.Raw file) =
    let
        changes : DeclarationsAndComments
        changes =
            List.foldl
                attachDocumentationAndFixOperators
                { declarations = []
                , previousComments = []
                , remainingComments = file.comments
                }
                file.declarations
    in
    { moduleDefinition = file.moduleDefinition
    , imports = file.imports
    , declarations = List.reverse changes.declarations
    , comments = List.sortWith (\(Node a _) (Node b _) -> Range.compare a b) (changes.remainingComments ++ changes.previousComments)
    }


type alias DeclarationsAndComments =
    { declarations : List (Node Declaration)
    , previousComments : List (Node Comment)
    , remainingComments : List (Node Comment)
    }


attachDocumentationAndFixOperators : Node Declaration -> DeclarationsAndComments -> DeclarationsAndComments
attachDocumentationAndFixOperators declaration context =
    case Node.value declaration of
        Declaration.FunctionDeclaration functionBeforeOperatorFix ->
            let
                function : Expression.Function
                function =
                    visitFunctionDecl functionBeforeOperatorFix
            in
            addDocumentation
                (\doc -> Declaration.FunctionDeclaration { function | documentation = Just doc })
                (Node (Node.range declaration) (Declaration.FunctionDeclaration function))
                context

        Declaration.AliasDeclaration typeAlias ->
            addDocumentation
                (\doc -> Declaration.AliasDeclaration { typeAlias | documentation = Just doc })
                declaration
                context

        Declaration.CustomTypeDeclaration typeDecl ->
            addDocumentation
                (\doc -> Declaration.CustomTypeDeclaration { typeDecl | documentation = Just doc })
                declaration
                context

        Declaration.PortDeclaration portDecl ->
            addDocumentation
                (\doc -> Declaration.PortDeclaration { portDecl | documentation = Just doc })
                declaration
                context

        Declaration.InfixDeclaration _ ->
            { previousComments = context.previousComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }


addDocumentation : (Node Comment -> Declaration) -> Node Declaration -> DeclarationsAndComments -> DeclarationsAndComments
addDocumentation howToUpdate declaration file =
    let
        ( previous, maybeDoc, remaining ) =
            findDocumentationForRange (Node.range declaration) file.remainingComments []
    in
    case maybeDoc of
        Just doc ->
            { previousComments = previous ++ file.previousComments
            , remainingComments = remaining
            , declarations = Node { start = (Node.range doc).start, end = (Node.range declaration).end } (howToUpdate doc) :: file.declarations
            }

        Nothing ->
            { previousComments = previous ++ file.previousComments
            , remainingComments = remaining
            , declarations = declaration :: file.declarations
            }


findDocumentationForRange : Range -> List (Node String) -> List (Node String) -> ( List (Node String), Maybe (Node String), List (Node String) )
findDocumentationForRange range comments previousComments =
    case comments of
        [] ->
            ( previousComments, Nothing, [] )

        ((Node commentRange commentText) as comment) :: restOfComments ->
            -- Since both comments and declarations are in the order that they appear in the source code,
            -- all the comments we've evaluated until now don't need to be re-evaluated when
            -- trying the find the documentation for later declarations if the current comment is later than the current declaration.
            case compare (commentRange.end.row + 1) range.start.row of
                EQ ->
                    if String.startsWith "{-|" commentText then
                        ( previousComments, Just comment, restOfComments )

                    else
                        -- Aborting because the next comment can't match the next declaration
                        ( previousComments, Nothing, comment :: restOfComments )

                LT ->
                    findDocumentationForRange range restOfComments (comment :: previousComments)

                GT ->
                    -- Aborting because we went too far
                    ( previousComments, Nothing, comment :: restOfComments )


fixApplication : List (Node Expression) -> Expression
fixApplication expressions =
    divideAndConquer (lowestPrecedence expressions) expressions


divideAndConquer : List ( String, SimpleInfix ) -> List (Node Expression) -> Expression
divideAndConquer ops exps =
    case ops of
        [] ->
            fixExprs exps

        op :: restOfOps ->
            case findNextSplit op restOfOps exps of
                Just ( p, infix_, s ) ->
                    Expression.Operation
                        infix_.operator
                        infix_.direction
                        (Node (Range.combine <| List.map Node.range p) (divideAndConquer ops p))
                        (Node (Range.combine <| List.map Node.range s) (divideAndConquer ops s))

                Nothing ->
                    fixExprs exps


fixExprs : List (Node Expression) -> Expression
fixExprs exps =
    case exps of
        [ Node _ x ] ->
            x

        head_ :: rest ->
            Expression.FunctionCall head_ rest

        [] ->
            -- This case should never happen
            Expression.TupleExpression []


findNextSplit : ( String, SimpleInfix ) -> List ( String, SimpleInfix ) -> List (Node Expression) -> Maybe ( List (Node Expression), SimpleInfix, List (Node Expression) )
findNextSplit op restOfOperators exps =
    let
        assocDirection : Infix.InfixDirection
        assocDirection =
            -- At this point we should ideally check if all operators have the same associativity
            -- and report an error if that's not the case.
            (Tuple.second op).direction

        operators : List ( String, SimpleInfix )
        operators =
            op :: restOfOperators

        prefix : List (Node Expression)
        prefix =
            case assocDirection of
                Infix.Left ->
                    exps
                        |> List.reverse
                        |> List.Extra.dropWhile
                            (\x ->
                                expressionOperators x
                                    |> Maybe.andThen (\key -> findInfix key operators)
                                    |> (==) Nothing
                            )
                        |> List.drop 1
                        |> List.reverse

                _ ->
                    exps
                        |> List.Extra.takeWhile
                            (\x ->
                                expressionOperators x
                                    |> Maybe.andThen (\key -> findInfix key operators)
                                    |> (==) Nothing
                            )

        prefixLength : Int
        prefixLength =
            List.length prefix
    in
    case
        exps
            |> List.drop prefixLength
            |> List.head
            |> Maybe.andThen expressionOperators
            |> Maybe.andThen (\x -> findInfix x operators)
    of
        Just x ->
            let
                suffix : List (Node Expression)
                suffix =
                    List.drop (prefixLength + 1) exps
            in
            Just ( prefix, x, suffix )

        Nothing ->
            Nothing


findInfix : String -> List ( String, SimpleInfix ) -> Maybe SimpleInfix
findInfix symbol list =
    case list of
        [] ->
            Nothing

        ( key, value ) :: rest ->
            if key == symbol then
                Just value

            else
                findInfix symbol rest


lowestPrecedence : List (Node Expression) -> List ( String, SimpleInfix )
lowestPrecedence expressions =
    let
        operatorsInArguments : List SimpleInfix
        operatorsInArguments =
            List.filterMap
                (\(Node _ expression) ->
                    case expression of
                        Expression.Operator symbol ->
                            Dict.get symbol Elm.Operators.bySymbol

                        _ ->
                            Nothing
                )
                expressions
    in
    case findMinimumPrecedence operatorsInArguments of
        Just m ->
            List.foldl
                (\infix_ acc ->
                    if infix_.precedence == m then
                        ( infix_.operator, infix_ ) :: acc

                    else
                        acc
                )
                []
                operatorsInArguments

        Nothing ->
            []


findMinimumPrecedence : List SimpleInfix -> Maybe Int
findMinimumPrecedence ops =
    case ops of
        [] ->
            Nothing

        x :: xs ->
            if x.precedence == 0 then
                Just 0

            else
                Just (findMinimumPrecedenceHelp x.precedence xs)


findMinimumPrecedenceHelp : Int -> List SimpleInfix -> Int
findMinimumPrecedenceHelp minPrecedence ops =
    case ops of
        [] ->
            minPrecedence

        x :: xs ->
            if x.precedence == 0 then
                0

            else
                findMinimumPrecedenceHelp (min minPrecedence x.precedence) xs


expressionOperators : Node Expression -> Maybe String
expressionOperators (Node _ expression) =
    case expression of
        Expression.Operator s ->
            Just s

        _ ->
            Nothing


visitLetDeclarations : List (Node Expression.LetDeclaration) -> List (Node Expression.LetDeclaration)
visitLetDeclarations declarations =
    List.map visitLetDeclaration declarations


visitLetDeclaration : Node Expression.LetDeclaration -> Node Expression.LetDeclaration
visitLetDeclaration (Node range declaration) =
    Node range <|
        case declaration of
            Expression.LetFunction function ->
                Expression.LetFunction (visitFunctionDecl function)

            Expression.LetDestructuring pattern expression ->
                Expression.LetDestructuring pattern (visitExpression expression)


visitFunctionDecl : Expression.Function -> Expression.Function
visitFunctionDecl function =
    let
        newFunctionDeclaration : Node Expression.FunctionImplementation
        newFunctionDeclaration =
            Node.map visitFunctionDeclaration function.declaration
    in
    { function | declaration = newFunctionDeclaration }


visitFunctionDeclaration : Expression.FunctionImplementation -> Expression.FunctionImplementation
visitFunctionDeclaration functionDeclaration =
    let
        newExpression : Node Expression
        newExpression =
            visitExpression functionDeclaration.expression
    in
    { functionDeclaration | expression = newExpression }


visitExpression : Node Expression -> Node Expression
visitExpression expression =
    visitExpressionInner <|
        case expression of
            Node r (Expression.FunctionCall function args) ->
                Node r (fixApplication (function :: args))

            _ ->
                expression


visitExpressionInner : Node Expression -> Node Expression
visitExpressionInner (Node range expression) =
    Node range <|
        case expression of
            Expression.FunctionCall function args ->
                Expression.FunctionCall (visitExpression function) (List.map visitExpression args)

            Expression.Operation op dir left right ->
                Expression.Operation op
                    dir
                    (visitExpression left)
                    (visitExpression right)

            Expression.If e1 e2 e3 ->
                Expression.If (visitExpression e1) (visitExpression e2) (visitExpression e3)

            Expression.TupleExpression expressionList ->
                expressionList
                    |> List.map visitExpression
                    |> Expression.TupleExpression

            Expression.Let letBlock ->
                Expression.Let
                    { declarations = visitLetDeclarations letBlock.declarations
                    , expression = visitExpression letBlock.expression
                    }

            Expression.Case caseBlock ->
                Expression.Case
                    { expression = visitExpression caseBlock.expression
                    , firstCase = Tuple.mapSecond visitExpression caseBlock.firstCase
                    , restOfCases = List.map (Tuple.mapSecond visitExpression) caseBlock.restOfCases
                    }

            Expression.LambdaExpression lambda ->
                Expression.LambdaExpression <| { lambda | expression = visitExpression lambda.expression }

            Expression.Record expressionStringList ->
                expressionStringList
                    |> List.map (Node.map (Tuple.mapSecond visitExpression))
                    |> Expression.Record

            Expression.ListLiteral expressionList ->
                Expression.ListLiteral (List.map visitExpression expressionList)

            Expression.RecordUpdate name firstUpdate updates ->
                Expression.RecordUpdate
                    name
                    (Node.map (Tuple.mapSecond visitExpression) firstUpdate)
                    (List.map (Node.map (Tuple.mapSecond visitExpression)) updates)

            Expression.Negation expr ->
                Expression.Negation (visitExpression expr)

            Expression.RecordAccess expr name ->
                Expression.RecordAccess (visitExpression expr) name

            _ ->
                expression
