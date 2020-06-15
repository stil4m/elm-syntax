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
import Elm.Internal.RawFile as RawFile exposing (RawFile(..))
import Elm.Processing.Documentation as Documentation
import Elm.RawFile as RawFile
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
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


{-| Add a whole depenency with its modules to the context.
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
        table =
            tableForFile rawFile processContext

        operatorFixed =
            visit
                (Just
                    (\context inner expression ->
                        inner <|
                            case expression of
                                Node r (Application args) ->
                                    Node r (fixApplication context args)

                                _ ->
                                    expression
                    )
                )
                table
                file

        documentationFixed =
            Documentation.postProcess operatorFixed
    in
    documentationFixed


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
        prefix =
            exps
                |> List.takeWhile
                    (\x ->
                        expressionOperators x
                            |> Maybe.andThen (\key -> Dict.get key dict)
                            |> (==) Nothing
                    )

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


type alias Visitor a =
    Maybe (a -> (Node Expression -> Node Expression) -> Node Expression -> Node Expression)


visit : Visitor context -> context -> File -> File
visit visitor context file =
    let
        newDeclarations =
            visitDeclarations visitor context file.declarations
    in
    { file | declarations = newDeclarations }


visitDeclarations : Visitor context -> context -> List (Node Declaration) -> List (Node Declaration)
visitDeclarations visitor context declarations =
    List.map (visitDeclaration visitor context) declarations


visitLetDeclarations : Visitor context -> context -> List (Node LetDeclaration) -> List (Node LetDeclaration)
visitLetDeclarations visitor context declarations =
    List.map (visitLetDeclaration visitor context) declarations


visitDeclaration : Visitor context -> context -> Node Declaration -> Node Declaration
visitDeclaration visitor context (Node range declaration) =
    Node range <|
        case declaration of
            FunctionDeclaration function ->
                FunctionDeclaration (visitFunctionDecl visitor context function)

            _ ->
                declaration


visitLetDeclaration : Visitor context -> context -> Node LetDeclaration -> Node LetDeclaration
visitLetDeclaration visitor context (Node range declaration) =
    Node range <|
        case declaration of
            LetFunction function ->
                LetFunction (visitFunctionDecl visitor context function)

            LetDestructuring pattern expression ->
                LetDestructuring pattern (visitExpression visitor context expression)


visitFunctionDecl : Visitor context -> context -> Function -> Function
visitFunctionDecl visitor context function =
    let
        newFunctionDeclaration =
            Node.map (visitFunctionDeclaration visitor context) function.declaration
    in
    { function | declaration = newFunctionDeclaration }


visitFunctionDeclaration : Visitor context -> context -> FunctionImplementation -> FunctionImplementation
visitFunctionDeclaration visitor context functionDeclaration =
    let
        newExpression =
            visitExpression visitor context functionDeclaration.expression
    in
    { functionDeclaration | expression = newExpression }


visitExpression : Visitor context -> context -> Node Expression -> Node Expression
visitExpression visitor context expression =
    let
        inner =
            visitExpressionInner visitor context
    in
    (visitor |> Maybe.withDefault (\_ nest expr -> nest expr))
        context
        inner
        expression


visitExpressionInner : Visitor context -> context -> Node Expression -> Node Expression
visitExpressionInner visitor context (Node range expression) =
    let
        subVisit =
            visitExpression visitor context
    in
    (\newExpr -> Node range newExpr) <|
        case expression of
            Application expressionList ->
                expressionList
                    |> List.map subVisit
                    |> Application

            OperatorApplication op dir left right ->
                OperatorApplication op
                    dir
                    (subVisit left)
                    (subVisit right)

            IfBlock e1 e2 e3 ->
                IfBlock (subVisit e1) (subVisit e2) (subVisit e3)

            TupledExpression expressionList ->
                expressionList
                    |> List.map subVisit
                    |> TupledExpression

            ParenthesizedExpression expr1 ->
                ParenthesizedExpression (subVisit expr1)

            LetExpression letBlock ->
                LetExpression
                    { declarations = visitLetDeclarations visitor context letBlock.declarations
                    , expression = subVisit letBlock.expression
                    }

            CaseExpression caseBlock ->
                CaseExpression
                    { expression = subVisit caseBlock.expression
                    , cases = List.map (Tuple.mapSecond subVisit) caseBlock.cases
                    }

            LambdaExpression lambda ->
                LambdaExpression <| { lambda | expression = subVisit lambda.expression }

            RecordExpr expressionStringList ->
                expressionStringList
                    |> List.map (Node.map (Tuple.mapSecond subVisit))
                    |> RecordExpr

            ListExpr expressionList ->
                ListExpr (List.map subVisit expressionList)

            RecordUpdateExpression name updates ->
                updates
                    |> List.map (Node.map (Tuple.mapSecond subVisit))
                    |> RecordUpdateExpression name

            _ ->
                expression
