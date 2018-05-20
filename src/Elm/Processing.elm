module Elm.Processing exposing (ProcessContext, addDependency, addFile, init, process)

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
import Elm.Syntax.Base exposing (ModuleName)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.Module exposing (Import)
import Elm.Syntax.Range as Range
import Elm.Syntax.Ranged exposing (Ranged)
import List exposing (maximum)
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
addFile file ((ProcessContext x) as m) =
    case entryFromRawFile file of
        Just ( k, v ) ->
            ProcessContext (Dict.insert k v x)

        Nothing ->
            m


{-| Add a whole depenency with its modules to the context.
-}
addDependency : Dependency -> ProcessContext -> ProcessContext
addDependency dep (ProcessContext x) =
    ProcessContext (Dict.foldl (\k v d -> Dict.insert k v d) x dep.interfaces)


entryFromRawFile : RawFile -> Maybe ( ModuleName, Interface )
entryFromRawFile ((Raw _) as rawFile) =
    case RawFile.moduleName rawFile of
        Just modName ->
            Just ( modName, Interface.build rawFile )

        Nothing ->
            Nothing


tableForFile : RawFile -> ProcessContext -> OperatorTable
tableForFile rawFile (ProcessContext moduleIndex) =
    List.concatMap (\a -> buildSingle a moduleIndex) (DefaultImports.defaults ++ RawFile.imports rawFile)
        |> Dict.fromList


buildSingle : Import -> ModuleIndexInner -> List ( String, Infix )
buildSingle imp moduleIndex =
    case imp.exposingList of
        Nothing ->
            []

        Just (All _) ->
            moduleIndex
                |> Dict.get imp.moduleName
                |> Maybe.withDefault []
                |> Interface.operators
                |> List.map (\x -> ( Tuple.second x.operator, x ))

        Just (Explicit l) ->
            let
                selectedOperators =
                    Exposing.operators <| List.map Tuple.second l
            in
            moduleIndex
                |> Dict.get imp.moduleName
                |> Maybe.withDefault []
                |> Interface.operators
                |> List.map (\x -> ( Tuple.second x.operator, x ))
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
                                ( r, Application args ) ->
                                    ( r, fixApplication context args )

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


fixApplication : OperatorTable -> List (Ranged Expression) -> Expression
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
                                { operator = ( Range.emptyRange, x )
                                , function = ( Range.emptyRange, "todo" )
                                , precedence = ( Range.emptyRange, 5 )
                                , direction = ( Range.emptyRange, Left )
                                }
                        )
                    )
                |> highestPrecedence

        fixExprs : List (Ranged Expression) -> Expression
        fixExprs exps =
            case exps of
                [ x ] ->
                    Tuple.second x

                _ ->
                    Application exps

        divideAndConquer : List (Ranged Expression) -> Expression
        divideAndConquer exps =
            if Dict.isEmpty ops then
                fixExprs exps

            else
                findNextSplit ops exps
                    |> Maybe.map
                        (\( p, infix, s ) ->
                            OperatorApplication
                                (Tuple.second infix.operator)
                                (Tuple.second infix.direction)
                                ( Range.combine <| List.map Tuple.first p, divideAndConquer p )
                                ( Range.combine <| List.map Tuple.first s, divideAndConquer s )
                        )
                    |> Maybe.withDefault (fixExprs exps)
    in
    divideAndConquer expressions


findNextSplit : Dict String Infix -> List (Ranged Expression) -> Maybe ( List (Ranged Expression), Infix, List (Ranged Expression) )
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


highestPrecedence : List ( String, Infix ) -> Dict String Infix
highestPrecedence input =
    let
        maxi =
            input
                |> List.map (Tuple.second >> .precedence >> Tuple.second)
                |> maximum
    in
    maxi
        |> Maybe.map (\m -> List.filter (Tuple.second >> .precedence >> Tuple.second >> (==) m) input)
        |> Maybe.withDefault []
        |> Dict.fromList


expressionOperators : Ranged Expression -> Maybe String
expressionOperators ( _, expression ) =
    case expression of
        Operator s ->
            Just s

        _ ->
            Nothing


type alias Visitor a =
    Maybe (a -> (Ranged Expression -> Ranged Expression) -> Ranged Expression -> Ranged Expression)


visit : Visitor context -> context -> File -> File
visit visitor context file =
    let
        newDeclarations =
            visitDeclarations visitor context file.declarations
    in
    { file | declarations = newDeclarations }


visitDeclarations : Visitor context -> context -> List (Ranged Declaration) -> List (Ranged Declaration)
visitDeclarations visitor context declarations =
    List.map (visitDeclaration visitor context) declarations


visitLetDeclarations : Visitor context -> context -> List (Ranged LetDeclaration) -> List (Ranged LetDeclaration)
visitLetDeclarations visitor context declarations =
    List.map (visitLetDeclaration visitor context) declarations


visitDeclaration : Visitor context -> context -> Ranged Declaration -> Ranged Declaration
visitDeclaration visitor context ( range, declaration ) =
    ( range
    , case declaration of
        FuncDecl function ->
            FuncDecl (visitFunctionDecl visitor context function)

        _ ->
            declaration
    )


visitLetDeclaration : Visitor context -> context -> Ranged LetDeclaration -> Ranged LetDeclaration
visitLetDeclaration visitor context ( range, declaration ) =
    ( range
    , case declaration of
        LetFunction function ->
            LetFunction (visitFunctionDecl visitor context function)

        LetDestructuring pattern expression ->
            LetDestructuring pattern (visitExpression visitor context expression)
    )


visitFunctionDecl : Visitor context -> context -> Function -> Function
visitFunctionDecl visitor context function =
    let
        newFunctionDeclaration =
            visitFunctionDeclaration visitor context function.declaration
    in
    { function | declaration = newFunctionDeclaration }


visitFunctionDeclaration : Visitor context -> context -> FunctionDeclaration -> FunctionDeclaration
visitFunctionDeclaration visitor context functionDeclaration =
    let
        newExpression =
            visitExpression visitor context functionDeclaration.expression
    in
    { functionDeclaration | expression = newExpression }


visitExpression : Visitor context -> context -> Ranged Expression -> Ranged Expression
visitExpression visitor context expression =
    let
        inner =
            visitExpressionInner visitor context
    in
    (visitor |> Maybe.withDefault (\_ nest expr -> nest expr))
        context
        inner
        expression


visitExpressionInner : Visitor context -> context -> Ranged Expression -> Ranged Expression
visitExpressionInner visitor context ( range, expression ) =
    let
        subVisit =
            visitExpression visitor context
    in
    (\newExpr -> ( range, newExpr )) <|
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
                    |> List.map (Tuple.mapSecond subVisit)
                    |> RecordExpr

            ListExpr expressionList ->
                ListExpr (List.map subVisit expressionList)

            RecordUpdateExpression recordUpdate ->
                recordUpdate.updates
                    |> List.map (Tuple.mapSecond subVisit)
                    |> (RecordUpdate recordUpdate.name >> RecordUpdateExpression)

            _ ->
                expression
