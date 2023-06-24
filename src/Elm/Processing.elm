module Elm.Processing exposing
    ( ProcessContext
    , init, addFile, addDependency, process
    )

{-| Processing raw files with the context of other files and dependencies.


## Types

@docs ProcessContext


## Functions

@docs init, addFile, addDependency, process

-}

import Dict exposing (Dict)
import Elm.Dependency exposing (Dependency)
import Elm.Interface as Interface exposing (Interface)
import Elm.Internal.RawFile as InternalRawFile
import Elm.OperatorTable
import Elm.RawFile as RawFile
import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import List
import List.Extra as List


{-| Opaque type to hold context for the processing
-}
type ProcessContext
    = ProcessContext ModuleIndexInner


type alias ModuleIndexInner =
    Dict ModuleName Interface


{-| Initialise an empty context
-}
init : ProcessContext
init =
    ProcessContext Dict.empty


{-| Add a file to the context that may be a dependency for the file that will be processed.
-}
addFile : RawFile.RawFile -> ProcessContext -> ProcessContext
addFile file (ProcessContext context) =
    ProcessContext
        (Dict.insert
            (RawFile.moduleName file)
            (Interface.build file)
            context
        )


{-| Add a whole dependency with its modules to the context.
-}
addDependency : Dependency -> ProcessContext -> ProcessContext
addDependency dep (ProcessContext x) =
    ProcessContext (Dict.union dep.interfaces x)


{-| Process a rawfile with a context.
Operator precedence and documentation will be fixed.
-}
process : ProcessContext -> RawFile.RawFile -> File
process _ (InternalRawFile.Raw file) =
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
        FunctionDeclaration functionBeforeOperatorFix ->
            let
                function : Function
                function =
                    visitFunctionDecl functionBeforeOperatorFix
            in
            addDocumentation
                (\doc -> FunctionDeclaration { function | documentation = Just doc })
                (Node (Node.range declaration) (FunctionDeclaration function))
                context

        AliasDeclaration typeAlias ->
            addDocumentation
                (\doc -> AliasDeclaration { typeAlias | documentation = Just doc })
                declaration
                context

        CustomTypeDeclaration typeDecl ->
            addDocumentation
                (\doc -> CustomTypeDeclaration { typeDecl | documentation = Just doc })
                declaration
                context

        PortDeclaration _ ->
            { previousComments = context.previousComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }

        InfixDeclaration _ ->
            { previousComments = context.previousComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }

        Destructuring _ _ ->
            -- Will never happen. Will be removed in v8
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
            , declarations = Node (Range.combine [ Node.range doc, Node.range declaration ]) (howToUpdate doc) :: file.declarations
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
    let
        ops : Dict String Infix
        ops =
            List.filterMap expressionOperators expressions
                |> List.map
                    (\x ->
                        ( x
                        , Dict.get x Elm.OperatorTable.table
                            |> Maybe.withDefault
                                { operator = Node Range.emptyRange x
                                , function = Node Range.emptyRange "todo"
                                , precedence = Node Range.emptyRange 5
                                , direction = Node Range.emptyRange Left
                                }
                        )
                    )
                |> lowestPrecedence

        fixExprs : List (Node Expression) -> Expression
        fixExprs exps =
            case exps of
                [ Node _ x ] ->
                    x

                _ ->
                    Application exps

        divideAndConquer : List (Node Expression) -> Expression
        divideAndConquer exps =
            if Dict.isEmpty ops then
                fixExprs exps

            else
                findNextSplit ops exps
                    |> Maybe.map
                        (\( p, infix_, s ) ->
                            OperatorApplication
                                (Node.value infix_.operator)
                                (Node.value infix_.direction)
                                (Node (Range.combine <| List.map Node.range p) (divideAndConquer p))
                                (Node (Range.combine <| List.map Node.range s) (divideAndConquer s))
                        )
                    |> Maybe.withDefault (fixExprs exps)
    in
    divideAndConquer expressions


findNextSplit : Dict String Infix -> List (Node Expression) -> Maybe ( List (Node Expression), Infix, List (Node Expression) )
findNextSplit dict exps =
    let
        assocDirection : InfixDirection
        assocDirection =
            dict
                |> Dict.toList
                |> List.map (Tuple.second >> .direction)
                -- At this point we should ideally check if all operators have the same associativity
                -- and report an error if that's not the case.
                |> List.head
                |> Maybe.map Node.value
                |> Maybe.withDefault Right

        prefix : List (Node Expression)
        prefix =
            case assocDirection of
                Left ->
                    exps
                        |> List.reverse
                        |> List.dropWhile
                            (\x ->
                                expressionOperators x
                                    |> Maybe.andThen (\key -> Dict.get key dict)
                                    |> (==) Nothing
                            )
                        |> List.drop 1
                        |> List.reverse

                _ ->
                    exps
                        |> List.takeWhile
                            (\x ->
                                expressionOperators x
                                    |> Maybe.andThen (\key -> Dict.get key dict)
                                    |> (==) Nothing
                            )
    in
    case
        exps
            |> List.drop (List.length prefix)
            |> List.head
            |> Maybe.andThen expressionOperators
            |> Maybe.andThen (\x -> Dict.get x dict)
    of
        Just x ->
            let
                suffix : List (Node Expression)
                suffix =
                    List.drop (List.length prefix + 1) exps
            in
            Just ( prefix, x, suffix )

        Nothing ->
            Nothing


lowestPrecedence : List ( String, Infix ) -> Dict String Infix
lowestPrecedence input =
    input
        |> List.map (Tuple.second >> .precedence >> Node.value)
        |> List.minimum
        |> Maybe.map (\m -> List.filter (Tuple.second >> .precedence >> Node.value >> (==) m) input)
        |> Maybe.withDefault []
        |> Dict.fromList


expressionOperators : Node Expression -> Maybe String
expressionOperators (Node _ expression) =
    case expression of
        Operator s ->
            Just s

        _ ->
            Nothing


visitLetDeclarations : List (Node LetDeclaration) -> List (Node LetDeclaration)
visitLetDeclarations declarations =
    List.map visitLetDeclaration declarations


visitLetDeclaration : Node LetDeclaration -> Node LetDeclaration
visitLetDeclaration (Node range declaration) =
    Node range <|
        case declaration of
            LetFunction function ->
                LetFunction (visitFunctionDecl function)

            LetDestructuring pattern expression ->
                LetDestructuring pattern (visitExpression expression)


visitFunctionDecl : Function -> Function
visitFunctionDecl function =
    let
        newFunctionDeclaration : Node FunctionImplementation
        newFunctionDeclaration =
            Node.map visitFunctionDeclaration function.declaration
    in
    { function | declaration = newFunctionDeclaration }


visitFunctionDeclaration : FunctionImplementation -> FunctionImplementation
visitFunctionDeclaration functionDeclaration =
    let
        newExpression : Node Expression
        newExpression =
            visitExpression functionDeclaration.expression
    in
    { functionDeclaration | expression = newExpression }


visitExpression : Node Expression -> Node Expression
visitExpression (Node range expression) =
    Node range <|
        case expression of
            Application args ->
                visitExpression (Node range (fixApplication args))
                    |> Node.value

            OperatorApplication op dir left right ->
                OperatorApplication op
                    dir
                    (visitExpression left)
                    (visitExpression right)

            IfBlock e1 e2 e3 ->
                IfBlock (visitExpression e1) (visitExpression e2) (visitExpression e3)

            TupledExpression expressionList ->
                expressionList
                    |> List.map visitExpression
                    |> TupledExpression

            ParenthesizedExpression expr1 ->
                ParenthesizedExpression (visitExpression expr1)

            LetExpression letBlock ->
                LetExpression
                    { declarations = visitLetDeclarations letBlock.declarations
                    , expression = visitExpression letBlock.expression
                    }

            CaseExpression caseBlock ->
                CaseExpression
                    { expression = visitExpression caseBlock.expression
                    , cases = List.map (Tuple.mapSecond visitExpression) caseBlock.cases
                    }

            LambdaExpression lambda ->
                LambdaExpression <| { lambda | expression = visitExpression lambda.expression }

            RecordExpr expressionStringList ->
                expressionStringList
                    |> List.map (Node.map (Tuple.mapSecond visitExpression))
                    |> RecordExpr

            ListExpr expressionList ->
                ListExpr (List.map visitExpression expressionList)

            RecordUpdateExpression name updates ->
                updates
                    |> List.map (Node.map (Tuple.mapSecond visitExpression))
                    |> RecordUpdateExpression name

            Negation expr ->
                Negation (visitExpression expr)

            RecordAccess expr name ->
                RecordAccess (visitExpression expr) name

            _ ->
                expression
