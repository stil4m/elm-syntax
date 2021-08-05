module Elm.Processing exposing
    ( ProcessContext
    , init, addFile, addDependency, process
    )

{-|


# Elm.Processing

Processing raw files with the context of other files and dependencies.


# Types

@docs ProcessContext


# Functions

@docs init, addFile, addDependency, process

-}

import Dict exposing (Dict)
import Elm.DefaultImports as DefaultImports
import Elm.Dependency exposing (Dependency)
import Elm.Interface as Interface exposing (Interface)
import Elm.Internal.RawFile exposing (RawFile(..))
import Elm.RawFile as RawFile
import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import List
import List.Extra as List


type alias OperatorTable =
    Dict String Infix


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
addFile : RawFile -> ProcessContext -> ProcessContext
addFile file (ProcessContext x) =
    let
        ( k, v ) =
            entryFromRawFile file
    in
    ProcessContext (Dict.insert k v x)


{-| Add a whole dependency with its modules to the context.
-}
addDependency : Dependency -> ProcessContext -> ProcessContext
addDependency dep (ProcessContext x) =
    ProcessContext (Dict.foldl (\k v d -> Dict.insert k v d) x dep.interfaces)


entryFromRawFile : RawFile -> ( ModuleName, Interface )
entryFromRawFile ((Raw _) as rawFile) =
    ( RawFile.moduleName rawFile, Interface.build rawFile )


tableForFile : RawFile -> ProcessContext -> OperatorTable
tableForFile rawFile (ProcessContext moduleIndex) =
    List.concatMap (\a -> buildSingle a moduleIndex) (DefaultImports.defaults ++ RawFile.imports rawFile)
        |> Dict.fromList


buildSingle : Import -> ModuleIndexInner -> List ( String, Infix )
buildSingle imp moduleIndex =
    case imp.exposingList of
        Nothing ->
            []

        Just (Node _ (All _)) ->
            moduleIndex
                |> Dict.get (Node.value imp.moduleName)
                |> Maybe.withDefault []
                |> Interface.operators
                |> List.map (\x -> ( Node.value x.operator, x ))

        Just (Node _ (Explicit l)) ->
            let
                selectedOperators : List String
                selectedOperators =
                    Exposing.operators <| List.map Node.value l
            in
            moduleIndex
                |> Dict.get (Node.value imp.moduleName)
                |> Maybe.withDefault []
                |> Interface.operators
                |> List.map (\x -> ( Node.value x.operator, x ))
                |> List.filter (Tuple.first >> (\elem -> List.member elem selectedOperators))


{-| Process a rawfile with a context.
Operator precedence and documentation will be fixed.
-}
process : ProcessContext -> RawFile -> File
process processContext ((Raw file) as rawFile) =
    let
        table : OperatorTable
        table =
            tableForFile rawFile processContext

        changes : ThingsToChange
        changes =
            List.foldl
                (attachDocumentationAndFixOperators table)
                { declarations = []
                , previousComments = []
                , remainingComments = file.comments
                }
                file.declarations
    in
    { moduleDefinition = file.moduleDefinition
    , imports = file.imports
    , declarations = List.reverse changes.declarations
    , comments =
        (changes.remainingComments :: changes.previousComments)
            |> List.reverse
            |> List.concat
    }


type alias ThingsToChange =
    { declarations : List (Node Declaration)
    , previousComments : List (List (Node Comment))
    , remainingComments : List (Node Comment)
    }


attachDocumentationAndFixOperators : OperatorTable -> Node Declaration -> ThingsToChange -> ThingsToChange
attachDocumentationAndFixOperators table declaration context =
    case Node.value declaration of
        FunctionDeclaration functionBeforeOperatorFix ->
            let
                function : Function
                function =
                    visitFunctionDecl table functionBeforeOperatorFix
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


addDocumentation : (Node Comment -> Declaration) -> Node Declaration -> ThingsToChange -> ThingsToChange
addDocumentation howToUpdate declaration file =
    let
        ( previous, maybeDoc, remaining ) =
            findDocumentationForRange (Node.range declaration) file.remainingComments []
    in
    case maybeDoc of
        Just doc ->
            { previousComments = previous :: file.previousComments
            , remainingComments = remaining
            , declarations = Node (Node.range declaration) (howToUpdate doc) :: file.declarations
            }

        Nothing ->
            { previousComments = previous :: file.previousComments
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


fixApplication : OperatorTable -> List (Node Expression) -> Expression
fixApplication operators expressions =
    let
        ops : Dict String Infix
        ops =
            List.filterMap expressionOperators expressions
                |> List.map
                    (\x ->
                        ( x
                        , Dict.get x operators
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
                        (\( p, infix, s ) ->
                            OperatorApplication
                                (Node.value infix.operator)
                                (Node.value infix.direction)
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

        suffix : List (Node Expression)
        suffix =
            List.drop (List.length prefix + 1) exps
    in
    exps
        |> List.drop (List.length prefix)
        |> List.head
        |> Maybe.andThen expressionOperators
        |> Maybe.andThen (\x -> Dict.get x dict)
        |> Maybe.map (\x -> ( prefix, x, suffix ))


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


type alias Visitor =
    (Node Expression -> Node Expression) -> Node Expression -> Node Expression


visitLetDeclarations : OperatorTable -> List (Node LetDeclaration) -> List (Node LetDeclaration)
visitLetDeclarations table declarations =
    List.map (visitLetDeclaration table) declarations


visitLetDeclaration : OperatorTable -> Node LetDeclaration -> Node LetDeclaration
visitLetDeclaration table (Node range declaration) =
    Node range <|
        case declaration of
            LetFunction function ->
                LetFunction (visitFunctionDecl table function)

            LetDestructuring pattern expression ->
                LetDestructuring pattern (visitExpression table expression)


visitFunctionDecl : OperatorTable -> Function -> Function
visitFunctionDecl table function =
    let
        newFunctionDeclaration : Node FunctionImplementation
        newFunctionDeclaration =
            Node.map (visitFunctionDeclaration table) function.declaration
    in
    { function | declaration = newFunctionDeclaration }


visitFunctionDeclaration : OperatorTable -> FunctionImplementation -> FunctionImplementation
visitFunctionDeclaration table functionDeclaration =
    let
        newExpression : Node Expression
        newExpression =
            visitExpression table functionDeclaration.expression
    in
    { functionDeclaration | expression = newExpression }


visitExpression : OperatorTable -> Node Expression -> Node Expression
visitExpression table expression =
    visitExpressionInner table <|
        case expression of
            Node r (Application args) ->
                Node r (fixApplication table args)

            _ ->
                expression


visitExpressionInner : OperatorTable -> Node Expression -> Node Expression
visitExpressionInner table (Node range expression) =
    Node range <|
        case expression of
            Application expressionList ->
                expressionList
                    |> List.map (visitExpression table)
                    |> Application

            OperatorApplication op dir left right ->
                OperatorApplication op
                    dir
                    (visitExpression table left)
                    (visitExpression table right)

            IfBlock e1 e2 e3 ->
                IfBlock (visitExpression table e1) (visitExpression table e2) (visitExpression table e3)

            TupledExpression expressionList ->
                expressionList
                    |> List.map (visitExpression table)
                    |> TupledExpression

            ParenthesizedExpression expr1 ->
                ParenthesizedExpression (visitExpression table expr1)

            LetExpression letBlock ->
                LetExpression
                    { declarations = visitLetDeclarations table letBlock.declarations
                    , expression = visitExpression table letBlock.expression
                    }

            CaseExpression caseBlock ->
                CaseExpression
                    { expression = visitExpression table caseBlock.expression
                    , cases = List.map (Tuple.mapSecond (visitExpression table)) caseBlock.cases
                    }

            LambdaExpression lambda ->
                LambdaExpression <| { lambda | expression = visitExpression table lambda.expression }

            RecordExpr expressionStringList ->
                expressionStringList
                    |> List.map (Node.map (Tuple.mapSecond (visitExpression table)))
                    |> RecordExpr

            ListExpr expressionList ->
                ListExpr (List.map (visitExpression table) expressionList)

            RecordUpdateExpression name updates ->
                updates
                    |> List.map (Node.map (Tuple.mapSecond (visitExpression table)))
                    |> RecordUpdateExpression name

            _ ->
                expression
