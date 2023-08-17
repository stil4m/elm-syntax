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
import StructuredWriter as Writer exposing (Writer, bracesComma, bracketsComma, breaked, epsilon, indent, parensComma, sepByComma, sepBySpace, spaced)


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
                [ Writer.string "port"
                , writeDefaultModuleData defaultModuleData
                ]

        Module.EffectModule effectModuleData ->
            writeEffectModuleData effectModuleData


writeDefaultModuleData : Module.DefaultModuleData -> Writer
writeDefaultModuleData { moduleName, exposingList } =
    spaced
        [ Writer.string "module"
        , writeModuleName <| Node.value moduleName
        , writeExposureExpose <| Node.value exposingList
        ]


writeEffectModuleData : Module.EffectModuleData -> Writer
writeEffectModuleData { moduleName, exposingList, command, subscription } =
    spaced
        [ Writer.string "effect"
        , Writer.string "module"
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
                [ Writer.string "where { command ="
                , Writer.string <| Node.value x
                , Writer.string "}"
                ]

        ( Nothing, Just x ) ->
            spaced
                [ Writer.string "where { subscription ="
                , Writer.string <| Node.value x
                , Writer.string "}"
                ]

        ( Just x, Just y ) ->
            spaced
                [ Writer.string "where { command ="
                , Writer.string <| Node.value x
                , Writer.string ", subscription ="
                , Writer.string <| Node.value y
                , Writer.string "}"
                ]


writeModuleName : ModuleName -> Writer
writeModuleName moduleName =
    Writer.string (String.join "." moduleName)


writeExposureExpose : Exposing -> Writer
writeExposureExpose x =
    case x of
        Exposing.All _ ->
            Writer.string "exposing (..)"

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
                [ Writer.string "exposing"
                , parensComma diffLines (List.map writeExpose exposeList)
                ]


writeExpose : Node Exposing.TopLevelExpose -> Writer
writeExpose (Node _ exp) =
    case exp of
        Exposing.InfixExpose x ->
            Writer.string ("(" ++ x ++ ")")

        Exposing.FunctionExpose f ->
            Writer.string f

        Exposing.TypeOrAliasExpose t ->
            Writer.string t

        Exposing.TypeExpose { name, open } ->
            case open of
                Just _ ->
                    spaced
                        [ Writer.string name
                        , Writer.string "(..)"
                        ]

                Nothing ->
                    Writer.string name


startOnDifferentLines : List Range -> Bool
startOnDifferentLines xs =
    List.length (List.Extra.unique (List.map (.start >> .row) xs)) > 1


writeImport : Import -> Writer
writeImport { moduleName, moduleAlias, exposingList } =
    spaced
        [ Writer.string "import"
        , writeModuleName <| Node.value moduleName
        , Writer.maybe (Maybe.map (Node.value >> writeModuleName >> (\x -> spaced [ Writer.string "as", x ])) moduleAlias)
        , Writer.maybe (Maybe.map (Node.value >> writeExposureExpose) exposingList)
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
        [ Writer.maybe (Maybe.map writeDocumentation documentation)
        , Writer.maybe (Maybe.map (Node.value >> writeSignature) signature)
        , writeFunctionImplementation <| Node.value declaration
        ]


writeFunctionImplementation : Expression.FunctionImplementation -> Writer
writeFunctionImplementation declaration =
    breaked
        [ spaced
            [ Writer.string <| Node.value declaration.name
            , spaced (List.map writeDestructurePattern declaration.arguments)
            , Writer.string "="
            ]
        , indent 4 (writeExpression declaration.expression)
        ]


writeSignature : Signature -> Writer
writeSignature signature =
    spaced
        [ Writer.string <| Node.value signature.name
        , Writer.string ":"
        , writeTypeAnnotation signature.typeAnnotation
        ]


writeDocumentation : Node Documentation -> Writer
writeDocumentation =
    Node.value >> Writer.string


writeTypeAlias : TypeAlias -> Writer
writeTypeAlias typeAlias =
    breaked
        [ spaced
            [ Writer.string "type alias"
            , Writer.string <| Node.value typeAlias.name
            , spaced (List.map (Node.value >> Writer.string) typeAlias.generics)
            , Writer.string "="
            ]
        , indent 4 (writeTypeAnnotation typeAlias.typeAnnotation)
        ]


writeType : Type -> Writer
writeType type_ =
    breaked
        [ spaced
            [ Writer.string "type"
            , Writer.string <| Node.value type_.name
            , spaced (List.map (Node.value >> Writer.string) type_.generics)
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
            (Writer.sepBy ( "= ", " | ", "" )
                diffLines
                (List.map (Node.value >> writeValueConstructor) constructors)
            )
        ]


writeValueConstructor : Type.ValueConstructor -> Writer
writeValueConstructor { name, arguments } =
    spaced
        ((Writer.string <| Node.value name)
            :: List.map (wrapInSurroundingParentheses >> writeTypeAnnotation) arguments
        )


writePortDeclaration : Port -> Writer
writePortDeclaration { signature, documentation } =
    case documentation of
        Just doc ->
            breaked
                [ writeDocumentation doc
                , spaced [ Writer.string "port", writeSignature <| Node.value signature ]
                ]

        Nothing ->
            spaced [ Writer.string "port", writeSignature <| Node.value signature ]


writeInfix : Infix -> Writer
writeInfix { direction, precedence, operator, function } =
    spaced
        [ Writer.string "infix"
        , case Node.value direction of
            Infix.Left ->
                Writer.string "left"

            Infix.Right ->
                Writer.string "right"

            Infix.Non ->
                Writer.string "non"
        , Writer.string (String.fromInt (Node.value precedence))
        , Writer.string (Node.value operator)
        , Writer.string "="
        , Writer.string (Node.value function)
        ]


writeDestructuring : Node DestructurePattern -> Node Expression -> Writer
writeDestructuring pattern expression =
    breaked
        [ spaced [ writeDestructurePattern pattern, Writer.string "=" ]
        , indent 4 (writeExpression expression)
        ]


{-| Write a type annotation
-}
writeTypeAnnotation : Node TypeAnnotation -> Writer
writeTypeAnnotation (Node _ typeAnnotation) =
    case typeAnnotation of
        TypeAnnotation.Var s ->
            Writer.string s

        TypeAnnotation.Type (Node _ ( moduleName, name )) args ->
            spaced
                ((Writer.string <| String.join "." (moduleName ++ [ name ]))
                    :: List.map (writeTypeAnnotation >> parensIfContainsSpaces) args
                )

        TypeAnnotation.Tuple xs ->
            parensComma False (List.map writeTypeAnnotation xs)

        TypeAnnotation.Record xs ->
            bracesComma False (List.map writeRecordField xs)

        TypeAnnotation.GenericRecord name fields ->
            spaced
                [ Writer.string "{"
                , Writer.string <| Node.value name
                , Writer.string "|"
                , sepByComma False (List.map writeRecordField <| Node.value fields)
                , Writer.string "}"
                ]

        TypeAnnotation.FunctionTypeAnnotation left right ->
            let
                addParensForSubTypeAnnotation : Node TypeAnnotation -> Writer
                addParensForSubTypeAnnotation type_ =
                    case type_ of
                        Node _ (TypeAnnotation.FunctionTypeAnnotation _ _) ->
                            Writer.join [ Writer.string "(", writeTypeAnnotation type_, Writer.string ")" ]

                        _ ->
                            writeTypeAnnotation type_
            in
            spaced
                [ addParensForSubTypeAnnotation left
                , Writer.string "->"
                , addParensForSubTypeAnnotation right
                ]


writeRecordField : Node TypeAnnotation.RecordField -> Writer
writeRecordField (Node _ ( name, ref )) =
    spaced
        [ Writer.string <| Node.value name
        , Writer.string ":"
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
            , spaced [ Writer.string <| Node.value name, Writer.string "=", writeExpression expr ]
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
        Expression.FunctionCall head xs ->
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
                        , ( range, spaced [ Writer.string x, writeExpression right ] )
                        ]

                Infix.Right ->
                    sepHelper sepBySpace
                        [ ( Node.range left, spaced [ writeExpression left, Writer.string x ] )
                        , ( Node.range right, writeExpression right )
                        ]

                Infix.Non ->
                    sepHelper sepBySpace
                        [ ( Node.range left, spaced [ writeExpression left, Writer.string x ] )
                        , ( Node.range right, writeExpression right )
                        ]

        Expression.FunctionOrValue moduleName name ->
            case moduleName of
                [] ->
                    Writer.string name

                _ ->
                    Writer.join
                        [ writeModuleName <| moduleName
                        , Writer.string "."
                        , Writer.string <| name
                        ]

        Expression.If condition thenCase elseCase ->
            breaked
                [ spaced [ Writer.string "if", writeExpression condition, Writer.string "then" ]
                , indent 2 (writeExpression thenCase)
                , Writer.string "else"
                , indent 2 (writeExpression elseCase)
                ]

        Expression.PrefixOperator x ->
            Writer.string ("(" ++ x ++ ")")

        Expression.Operator x ->
            Writer.string x

        Expression.HexLiteral h ->
            Writer.join [ Writer.string "0x", Writer.string (Hex.toString h) ]

        Expression.IntegerLiteral i ->
            Writer.string (String.fromInt i)

        Expression.FloatLiteral f ->
            Writer.string (String.fromFloat f)

        Expression.Negation x ->
            Writer.append (Writer.string "-") (writeExpression x)

        Expression.StringLiteral Expression.SingleQuote s ->
            Writer.string ("\"" ++ s ++ "\"")

        Expression.StringLiteral Expression.TripleQuote s ->
            Writer.string ("\"\"\"" ++ s ++ "\"\"\"")

        Expression.CharLiteral c ->
            writeChar c

        Expression.TupleExpression t ->
            Writer.join [ Writer.string "(", sepHelper Writer.sepByComma (List.map recurRangeHelper t), Writer.string ")" ]

        Expression.Let letBlock ->
            breaked
                [ Writer.string "let"
                , indent 2 (breaked (List.map writeLetDeclaration letBlock.declarations))
                , Writer.string "in"
                , indent 2 (writeExpression letBlock.expression)
                ]

        Expression.Case caseBlock ->
            let
                writeCaseBranch : ( Node Pattern, Node Expression ) -> Writer
                writeCaseBranch ( pattern, expression ) =
                    indent 2 <|
                        breaked
                            [ spaced [ writePattern pattern, Writer.string "->" ]
                            , indent 2 (writeExpression expression)
                            ]
            in
            breaked
                [ Writer.string ""
                , spaced [ Writer.string "case", writeExpression caseBlock.expression, Writer.string "of" ]
                , breaked (List.map writeCaseBranch (caseBlock.firstCase :: caseBlock.restOfCases))
                , Writer.string ""
                ]

        Expression.LambdaExpression lambda ->
            spaced
                [ Writer.join
                    [ Writer.string "\\"
                    , spaced (List.map writeDestructurePattern (lambda.firstArg :: lambda.restOfArgs))
                    ]
                , Writer.string "->"
                , writeExpression lambda.expression
                ]

        Expression.Record setters ->
            sepHelper Writer.bracesComma (List.map (Node.value >> writeRecordSetter) setters)

        Expression.ListLiteral xs ->
            sepHelper Writer.bracketsComma (List.map recurRangeHelper xs)

        Expression.RecordAccess expression accessor ->
            Writer.join [ writeExpression expression, Writer.string ".", Writer.string <| Node.value accessor ]

        Expression.RecordAccessFunction s ->
            if String.startsWith "." s then
                Writer.string s

            else
                Writer.join [ Writer.string ".", Writer.string s ]

        Expression.RecordUpdate name firstUpdate updates ->
            spaced
                [ Writer.string "{"
                , Writer.string <| Node.value name
                , Writer.string "|"
                , sepHelper Writer.sepByComma (List.map (Node.value >> writeRecordSetter) (firstUpdate :: updates))
                , Writer.string "}"
                ]

        Expression.GLSL s ->
            Writer.join
                [ Writer.string "[glsl|"
                , Writer.string s
                , Writer.string "|]"
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
    Writer.string ("'" ++ escape ++ String.fromChar c ++ "'")


{-| Write a pattern
-}
writePattern : Node Pattern -> Writer
writePattern (Node _ p) =
    case p of
        Pattern.AllPattern ->
            Writer.string "_"

        Pattern.UnitPattern ->
            Writer.string "()"

        Pattern.CharPattern c ->
            writeChar c

        Pattern.StringPattern s ->
            Writer.string ("\"" ++ escapeString s ++ "\"")

        Pattern.HexPattern h ->
            Writer.join [ Writer.string "0x", Writer.string (Hex.toString h) ]

        Pattern.IntPattern i ->
            Writer.string (String.fromInt i)

        Pattern.TuplePattern inner ->
            parensComma False (List.map writePattern inner)

        Pattern.RecordPattern inner ->
            bracesComma False (List.map (Node.value >> Writer.string) inner)

        Pattern.UnConsPattern left right ->
            spaced [ writePattern left, Writer.string "::", writePattern right ]

        Pattern.ListPattern inner ->
            bracketsComma False (List.map writePattern inner)

        Pattern.VarPattern var ->
            Writer.string var

        Pattern.NamedPattern qnr others ->
            spaced
                [ writeQualifiedNameRef qnr
                , spaced (List.map writePattern others)
                ]

        Pattern.AsPattern innerPattern asName ->
            spaced [ writePattern innerPattern, Writer.string "as", Writer.string <| Node.value asName ]

        Pattern.ParenthesizedPattern innerPattern ->
            spaced [ Writer.string "(", writePattern innerPattern, Writer.string ")" ]


writeDestructurePattern : Node DestructurePattern -> Writer
writeDestructurePattern (Node _ p) =
    case p of
        DestructurePattern.AllPattern_ ->
            Writer.string "_"

        DestructurePattern.UnitPattern_ ->
            Writer.string "()"

        DestructurePattern.TuplePattern_ inner ->
            parensComma False (List.map writeDestructurePattern inner)

        DestructurePattern.RecordPattern_ inner ->
            bracesComma False (List.map (Node.value >> Writer.string) inner)

        DestructurePattern.VarPattern_ var ->
            Writer.string var

        DestructurePattern.NamedPattern_ qnr others ->
            spaced
                [ writeQualifiedNameRef qnr
                , spaced (List.map writeDestructurePattern others)
                ]

        DestructurePattern.AsPattern_ innerPattern asName ->
            spaced [ writeDestructurePattern innerPattern, Writer.string "as", Writer.string <| Node.value asName ]

        DestructurePattern.ParenthesizedPattern_ innerPattern ->
            spaced [ Writer.string "(", writeDestructurePattern innerPattern, Writer.string ")" ]


writeQualifiedNameRef : Pattern.QualifiedNameRef -> Writer
writeQualifiedNameRef { moduleName, name } =
    case moduleName of
        [] ->
            Writer.string name

        _ ->
            Writer.join
                [ writeModuleName moduleName
                , Writer.string "."
                , Writer.string name
                ]



-- Helpers


parensIfContainsSpaces : Writer -> Writer
parensIfContainsSpaces w =
    if Writer.write w |> String.contains " " then
        Writer.join [ Writer.string "(", w, Writer.string ")" ]

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
