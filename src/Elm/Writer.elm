module Elm.Writer exposing (write, writeFile, writePattern, writeExpression, writeTypeAnnotation, writeDeclaration)

{-| Write a file to a string.

**DEPRECATED:** In practice the writer is not very good, and this functionality will very likely be removed in the next major version.
We highly recommend using [the-sett/elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest/) instead.

@docs write, writeFile, writePattern, writeExpression, writeTypeAnnotation, writeDeclaration

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.DestructurePattern as DestructurePattern exposing (DestructurePattern)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix as Infix exposing (Infix)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Port exposing (Port)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Hex
import List.Extra
import StructuredWriter as Writer exposing (..)


{-| Transform a writer to a string
-}
write : Writer -> String
write =
    Writer.write


{-| Write a file
-}
writeFile : File -> Writer
writeFile file =
    breaked
        [ writeModule <| Node.value file.moduleDefinition
        , breaked (List.map (Node.value >> writeImport) file.imports)
        , breaked (List.map writeDeclaration file.declarations)
        ]


writeModule : Module -> Writer
writeModule m =
    case m of
        Module.NormalModule defaultModuleData ->
            writeDefaultModuleData defaultModuleData

        Module.PortModule defaultModuleData ->
            spaced
                [ string "port"
                , writeDefaultModuleData defaultModuleData
                ]

        Module.EffectModule effectModuleData ->
            writeEffectModuleData effectModuleData


writeDefaultModuleData : Module.DefaultModuleData -> Writer
writeDefaultModuleData { moduleName, exposingList } =
    spaced
        [ string "module"
        , writeModuleName <| Node.value moduleName
        , writeExposureExpose <| Node.value exposingList
        ]


writeEffectModuleData : Module.EffectModuleData -> Writer
writeEffectModuleData { moduleName, exposingList, command, subscription } =
    spaced
        [ string "effect"
        , string "module"
        , writeModuleName <| Node.value moduleName
        , writeWhere ( command, subscription )
        , writeExposureExpose <| Node.value exposingList
        ]


writeWhere : ( Maybe (Node String), Maybe (Node String) ) -> Writer
writeWhere input =
    case input of
        ( Nothing, Nothing ) ->
            epsilon

        ( Just x, Nothing ) ->
            spaced
                [ string "where { command ="
                , string <| Node.value x
                , string "}"
                ]

        ( Nothing, Just x ) ->
            spaced
                [ string "where { subscription ="
                , string <| Node.value x
                , string "}"
                ]

        ( Just x, Just y ) ->
            spaced
                [ string "where { command ="
                , string <| Node.value x
                , string ", subscription ="
                , string <| Node.value y
                , string "}"
                ]


writeModuleName : ModuleName -> Writer
writeModuleName moduleName =
    string (String.join "." moduleName)


writeExposureExpose : Exposing -> Writer
writeExposureExpose x =
    case x of
        Exposing.All _ ->
            string "exposing (..)"

        Exposing.Explicit head rest ->
            let
                exposeList : List (Node Exposing.TopLevelExpose)
                exposeList =
                    head :: rest

                diffLines : Bool
                diffLines =
                    List.map Node.range exposeList
                        |> startOnDifferentLines
            in
            spaced
                [ string "exposing"
                , parensComma diffLines (List.map writeExpose exposeList)
                ]


writeExpose : Node Exposing.TopLevelExpose -> Writer
writeExpose (Node _ exp) =
    case exp of
        Exposing.InfixExpose x ->
            string ("(" ++ x ++ ")")

        Exposing.FunctionExpose f ->
            string f

        Exposing.TypeOrAliasExpose t ->
            string t

        Exposing.TypeExpose { name, open } ->
            case open of
                Just _ ->
                    spaced
                        [ string name
                        , string "(..)"
                        ]

                Nothing ->
                    string name


startOnDifferentLines : List Range -> Bool
startOnDifferentLines xs =
    List.length (List.Extra.unique (List.map (.start >> .row) xs)) > 1


writeImport : Import -> Writer
writeImport { moduleName, moduleAlias, exposingList } =
    spaced
        [ string "import"
        , writeModuleName <| Node.value moduleName
        , maybe (Maybe.map (Node.value >> writeModuleName >> (\x -> spaced [ string "as", x ])) moduleAlias)
        , maybe (Maybe.map (Node.value >> writeExposureExpose) exposingList)
        ]


writeLetDeclaration : Node Expression.LetDeclaration -> Writer
writeLetDeclaration (Node _ letDeclaration) =
    case letDeclaration of
        Expression.LetFunction function ->
            writeFunction function

        Expression.LetDestructuring pattern expression ->
            writeDestructuring pattern expression


{-| Write a declaration
-}
writeDeclaration : Node Declaration -> Writer
writeDeclaration (Node _ decl) =
    case decl of
        Declaration.FunctionDeclaration function ->
            writeFunction function

        Declaration.AliasDeclaration typeAlias ->
            writeTypeAlias typeAlias

        Declaration.CustomTypeDeclaration type_ ->
            writeType type_

        Declaration.PortDeclaration p ->
            writePortDeclaration p

        Declaration.InfixDeclaration i ->
            writeInfix i


writeFunction : Expression.Function -> Writer
writeFunction { documentation, signature, declaration } =
    breaked
        [ maybe (Maybe.map writeDocumentation documentation)
        , maybe (Maybe.map (Node.value >> writeSignature) signature)
        , writeFunctionImplementation <| Node.value declaration
        ]


writeFunctionImplementation : Expression.FunctionImplementation -> Writer
writeFunctionImplementation declaration =
    breaked
        [ spaced
            [ string <| Node.value declaration.name
            , spaced (List.map writeDestructurePattern declaration.arguments)
            , string "="
            ]
        , indent 4 (writeExpression declaration.expression)
        ]


writeSignature : Signature -> Writer
writeSignature signature =
    spaced
        [ string <| Node.value signature.name
        , string ":"
        , writeTypeAnnotation signature.typeAnnotation
        ]


writeDocumentation : Node Documentation -> Writer
writeDocumentation =
    Node.value >> string


writeTypeAlias : TypeAlias -> Writer
writeTypeAlias typeAlias =
    breaked
        [ spaced
            [ string "type alias"
            , string <| Node.value typeAlias.name
            , spaced (List.map (Node.value >> string) typeAlias.generics)
            , string "="
            ]
        , indent 4 (writeTypeAnnotation typeAlias.typeAnnotation)
        ]


writeType : Type -> Writer
writeType type_ =
    breaked
        [ spaced
            [ string "type"
            , string <| Node.value type_.name
            , spaced (List.map (Node.value >> string) type_.generics)
            ]
        , let
            constructors : List (Node Type.ValueConstructor)
            constructors =
                type_.firstConstructor :: type_.restOfConstructors

            diffLines : Bool
            diffLines =
                List.map Node.range constructors
                    |> startOnDifferentLines
          in
          indent 4
            (sepBy ( "= ", " | ", "" )
                diffLines
                (List.map (Node.value >> writeValueConstructor) constructors)
            )
        ]


writeValueConstructor : Type.ValueConstructor -> Writer
writeValueConstructor { name, arguments } =
    spaced
        ((string <| Node.value name)
            :: List.map (wrapInSurroundingParentheses >> writeTypeAnnotation) arguments
        )


writePortDeclaration : Port -> Writer
writePortDeclaration { signature, documentation } =
    case documentation of
        Just doc ->
            breaked
                [ writeDocumentation doc
                , spaced [ string "port", writeSignature <| Node.value signature ]
                ]

        Nothing ->
            spaced [ string "port", writeSignature <| Node.value signature ]


writeInfix : Infix -> Writer
writeInfix { direction, precedence, operator, function } =
    spaced
        [ string "infix"
        , case Node.value direction of
            Infix.Left ->
                string "left"

            Infix.Right ->
                string "right"

            Infix.Non ->
                string "non"
        , string (String.fromInt (Node.value precedence))
        , string (Node.value operator)
        , string "="
        , string (Node.value function)
        ]


writeDestructuring : Node DestructurePattern -> Node Expression -> Writer
writeDestructuring pattern expression =
    breaked
        [ spaced [ writeDestructurePattern pattern, string "=" ]
        , indent 4 (writeExpression expression)
        ]


{-| Write a type annotation
-}
writeTypeAnnotation : Node TypeAnnotation -> Writer
writeTypeAnnotation (Node _ typeAnnotation) =
    case typeAnnotation of
        TypeAnnotation.Var s ->
            string s

        TypeAnnotation.Type (Node _ ( moduleName, name )) args ->
            spaced
                ((string <| String.join "." (moduleName ++ [ name ]))
                    :: List.map (writeTypeAnnotation >> parensIfContainsSpaces) args
                )

        TypeAnnotation.Tuple xs ->
            parensComma False (List.map writeTypeAnnotation xs)

        TypeAnnotation.Record xs ->
            bracesComma False (List.map writeRecordField xs)

        TypeAnnotation.GenericRecord name fields ->
            spaced
                [ string "{"
                , string <| Node.value name
                , string "|"
                , sepByComma False (List.map writeRecordField <| Node.value fields)
                , string "}"
                ]

        TypeAnnotation.FunctionTypeAnnotation left right ->
            let
                addParensForSubTypeAnnotation : Node TypeAnnotation -> Writer
                addParensForSubTypeAnnotation type_ =
                    case type_ of
                        Node _ (TypeAnnotation.FunctionTypeAnnotation _ _) ->
                            join [ string "(", writeTypeAnnotation type_, string ")" ]

                        _ ->
                            writeTypeAnnotation type_
            in
            spaced
                [ addParensForSubTypeAnnotation left
                , string "->"
                , addParensForSubTypeAnnotation right
                ]


writeRecordField : Node TypeAnnotation.RecordField -> Writer
writeRecordField (Node _ ( name, ref )) =
    spaced
        [ string <| Node.value name
        , string ":"
        , writeTypeAnnotation ref
        ]


{-| Writer an expression
-}
writeExpression : Node Expression -> Writer
writeExpression (Node range inner) =
    let
        recurRangeHelper : Node Expression -> ( Range, Writer )
        recurRangeHelper (Node x y) =
            ( x, writeExpression (Node x y) )

        writeRecordSetter : Expression.RecordSetter -> ( Range, Writer )
        writeRecordSetter ( name, expr ) =
            ( Node.range expr
            , spaced [ string <| Node.value name, string "=", writeExpression expr ]
            )

        sepHelper : (Bool -> List Writer -> Writer) -> List ( Range, Writer ) -> Writer
        sepHelper f l =
            let
                diffLines : Bool
                diffLines =
                    List.map Tuple.first l
                        |> startOnDifferentLines
            in
            f diffLines (List.map Tuple.second l)
    in
    case inner of
        Expression.Application head xs ->
            case xs of
                [] ->
                    writeExpression head

                rest ->
                    spaced
                        [ writeExpression head
                        , sepHelper sepBySpace (List.map recurRangeHelper rest)
                        ]

        Expression.Operation x dir left right ->
            case dir of
                Infix.Left ->
                    sepHelper sepBySpace
                        [ ( Node.range left, writeExpression left )
                        , ( range, spaced [ string x, writeExpression right ] )
                        ]

                Infix.Right ->
                    sepHelper sepBySpace
                        [ ( Node.range left, spaced [ writeExpression left, string x ] )
                        , ( Node.range right, writeExpression right )
                        ]

                Infix.Non ->
                    sepHelper sepBySpace
                        [ ( Node.range left, spaced [ writeExpression left, string x ] )
                        , ( Node.range right, writeExpression right )
                        ]

        Expression.FunctionOrValue moduleName name ->
            case moduleName of
                [] ->
                    string name

                _ ->
                    join
                        [ writeModuleName <| moduleName
                        , string "."
                        , string <| name
                        ]

        Expression.If condition thenCase elseCase ->
            breaked
                [ spaced [ string "if", writeExpression condition, string "then" ]
                , indent 2 (writeExpression thenCase)
                , string "else"
                , indent 2 (writeExpression elseCase)
                ]

        Expression.PrefixOperator x ->
            string ("(" ++ x ++ ")")

        Expression.Operator x ->
            string x

        Expression.HexLiteral h ->
            join [ string "0x", string (Hex.toString h) ]

        Expression.IntegerLiteral i ->
            string (String.fromInt i)

        Expression.FloatLiteral f ->
            string (String.fromFloat f)

        Expression.Negation x ->
            append (string "-") (writeExpression x)

        Expression.StringLiteral Expression.SingleQuote s ->
            string ("\"" ++ s ++ "\"")

        Expression.StringLiteral Expression.TripleQuote s ->
            string ("\"\"\"" ++ s ++ "\"\"\"")

        Expression.CharLiteral c ->
            writeChar c

        Expression.TupleExpression t ->
            join [ string "(", sepHelper sepByComma (List.map recurRangeHelper t), string ")" ]

        Expression.LetExpression letBlock ->
            breaked
                [ string "let"
                , indent 2 (breaked (List.map writeLetDeclaration letBlock.declarations))
                , string "in"
                , indent 2 (writeExpression letBlock.expression)
                ]

        Expression.CaseExpression caseBlock ->
            let
                writeCaseBranch : ( Node Pattern, Node Expression ) -> Writer
                writeCaseBranch ( pattern, expression ) =
                    indent 2 <|
                        breaked
                            [ spaced [ writePattern pattern, string "->" ]
                            , indent 2 (writeExpression expression)
                            ]
            in
            breaked
                [ string ""
                , spaced [ string "case", writeExpression caseBlock.expression, string "of" ]
                , breaked (List.map writeCaseBranch (caseBlock.firstCase :: caseBlock.restOfCases))
                , string ""
                ]

        Expression.LambdaExpression lambda ->
            spaced
                [ join
                    [ string "\\"
                    , spaced (List.map writeDestructurePattern (lambda.firstArg :: lambda.restOfArgs))
                    ]
                , string "->"
                , writeExpression lambda.expression
                ]

        Expression.RecordExpr setters ->
            sepHelper bracesComma (List.map (Node.value >> writeRecordSetter) setters)

        Expression.ListLiteral xs ->
            sepHelper bracketsComma (List.map recurRangeHelper xs)

        Expression.RecordAccess expression accessor ->
            join [ writeExpression expression, string ".", string <| Node.value accessor ]

        Expression.RecordAccessFunction s ->
            if String.startsWith "." s then
                string s

            else
                join [ string ".", string s ]

        Expression.RecordUpdate name firstUpdate updates ->
            spaced
                [ string "{"
                , string <| Node.value name
                , string "|"
                , sepHelper sepByComma (List.map (Node.value >> writeRecordSetter) (firstUpdate :: updates))
                , string "}"
                ]

        Expression.GLSLExpression s ->
            join
                [ string "[glsl|"
                , string s
                , string "|]"
                ]


escapeString : String -> String
escapeString =
    String.replace "\"" "\\\""


writeChar : Char -> Writer
writeChar c =
    let
        escape : String
        escape =
            if c == '\t' || c == '\'' || c == '\\' then
                "\\"

            else
                ""
    in
    string ("'" ++ escape ++ String.fromChar c ++ "'")


{-| Write a pattern
-}
writePattern : Node Pattern -> Writer
writePattern (Node _ p) =
    case p of
        Pattern.AllPattern ->
            string "_"

        Pattern.UnitPattern ->
            string "()"

        Pattern.CharPattern c ->
            writeChar c

        Pattern.StringPattern s ->
            string ("\"" ++ escapeString s ++ "\"")

        Pattern.HexPattern h ->
            join [ string "0x", string (Hex.toString h) ]

        Pattern.IntPattern i ->
            string (String.fromInt i)

        Pattern.TuplePattern inner ->
            parensComma False (List.map writePattern inner)

        Pattern.RecordPattern inner ->
            bracesComma False (List.map (Node.value >> string) inner)

        Pattern.UnConsPattern left right ->
            spaced [ writePattern left, string "::", writePattern right ]

        Pattern.ListPattern inner ->
            bracketsComma False (List.map writePattern inner)

        Pattern.VarPattern var ->
            string var

        Pattern.NamedPattern qnr others ->
            spaced
                [ writeQualifiedNameRef qnr
                , spaced (List.map writePattern others)
                ]

        Pattern.AsPattern innerPattern asName ->
            spaced [ writePattern innerPattern, string "as", string <| Node.value asName ]

        Pattern.ParenthesizedPattern innerPattern ->
            spaced [ string "(", writePattern innerPattern, string ")" ]


writeDestructurePattern : Node DestructurePattern -> Writer
writeDestructurePattern (Node _ p) =
    case p of
        DestructurePattern.AllPattern_ ->
            string "_"

        DestructurePattern.UnitPattern_ ->
            string "()"

        DestructurePattern.TuplePattern_ inner ->
            parensComma False (List.map writeDestructurePattern inner)

        DestructurePattern.RecordPattern_ inner ->
            bracesComma False (List.map (Node.value >> string) inner)

        DestructurePattern.VarPattern_ var ->
            string var

        DestructurePattern.NamedPattern_ qnr others ->
            spaced
                [ writeQualifiedNameRef qnr
                , spaced (List.map writeDestructurePattern others)
                ]

        DestructurePattern.AsPattern_ innerPattern asName ->
            spaced [ writeDestructurePattern innerPattern, string "as", string <| Node.value asName ]

        DestructurePattern.ParenthesizedPattern_ innerPattern ->
            spaced [ string "(", writeDestructurePattern innerPattern, string ")" ]


writeQualifiedNameRef : Pattern.QualifiedNameRef -> Writer
writeQualifiedNameRef { moduleName, name } =
    case moduleName of
        [] ->
            string name

        _ ->
            join
                [ writeModuleName moduleName
                , string "."
                , string name
                ]



-- Helpers


parensIfContainsSpaces : Writer -> Writer
parensIfContainsSpaces w =
    if Writer.write w |> String.contains " " then
        join [ string "(", w, string ")" ]

    else
        w


wrapInSurroundingParentheses : Node TypeAnnotation -> Node TypeAnnotation
wrapInSurroundingParentheses node =
    let
        withParens : Node TypeAnnotation -> Node TypeAnnotation
        withParens n =
            Node.empty (TypeAnnotation.Tuple [ n ])
    in
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation _ _ ->
            withParens node

        TypeAnnotation.Type _ typeParameters ->
            case typeParameters of
                [] ->
                    node

                _ ->
                    withParens node

        _ ->
            node
