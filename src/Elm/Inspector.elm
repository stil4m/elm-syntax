module Elm.Inspector exposing (Config, inspect)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))


type alias Config context =
    { onFunction : Node Function -> context -> context
    , onTypeAlias : Node TypeAlias -> context -> context
    , onType : Node Type -> context -> context
    , onPortDeclaration : Node Signature -> context -> context
    }


actionLambda : (x -> context -> context) -> (context -> context) -> x -> context -> context
actionLambda g f x c =
    f c |> g x


ignoreSomething : (c -> a) -> b -> c -> a
ignoreSomething =
    \f _ c -> f c


inspect : Config a -> File -> a -> a
inspect config file context =
    inspectDeclarations config file.declarations context


inspectDeclarations : Config context -> List (Node Declaration) -> context -> context
inspectDeclarations config declarations context =
    List.foldl (inspectDeclaration config) context declarations


inspectLetDeclarations : Config context -> List (Node LetDeclaration) -> context -> context
inspectLetDeclarations config declarations context =
    List.foldl (inspectLetDeclaration config) context declarations


inspectLetDeclaration : Config context -> Node LetDeclaration -> context -> context
inspectLetDeclaration config (Node range declaration) context =
    case declaration of
        LetFunction function ->
            inspectFunction config (Node range function) context

        LetDestructuring pattern expression ->
            inspectDestructuring config (Node range ( pattern, expression )) context


inspectDeclaration : Config context -> Node Declaration -> context -> context
inspectDeclaration config (Node r declaration) context =
    case declaration of
        FunctionDeclaration function ->
            inspectFunction config (Node r function) context

        AliasDeclaration typeAlias ->
            inspectTypeAlias config (Node r typeAlias) context

        CustomTypeDeclaration typeDecl ->
            inspectType config (Node r typeDecl) context

        PortDeclaration signature ->
            inspectSignature config (Node r signature) context

        InfixDeclaration _ ->
            context

        Destructuring _ _ ->
            -- Will never happen. Will be removed in v8
            context


inspectType : Config context -> Node Type -> context -> context
inspectType config tipe context =
    actionLambda
        config.onType
        (inspectTypeInner config <| Node.value tipe)
        tipe
        context


inspectTypeInner : Config context -> Type -> context -> context
inspectTypeInner config typeDecl context =
    List.foldl (inspectValueConstructor config) context typeDecl.constructors


inspectValueConstructor : Config context -> Node ValueConstructor -> context -> context
inspectValueConstructor config (Node _ valueConstructor) context =
    List.foldl (inspectTypeAnnotation config) context valueConstructor.arguments


inspectTypeAlias : Config context -> Node TypeAlias -> context -> context
inspectTypeAlias config ((Node _ typeAlias) as pair) context =
    actionLambda
        config.onTypeAlias
        (inspectTypeAnnotation config typeAlias.typeAnnotation)
        pair
        context


inspectDestructuring : Config context -> Node ( Node Pattern, Node Expression ) -> context -> context
inspectDestructuring config destructuring context =
    inspectExpression
        config
        (Tuple.second <| Node.value destructuring)
        context


inspectFunction : Config context -> Node Function -> context -> context
inspectFunction config ((Node _ function) as node) context =
    actionLambda
        config.onFunction
        (inspectExpression config (Node.value function.declaration).expression
            >> (Maybe.withDefault identity <|
                    Maybe.map (inspectSignature config) function.signature
               )
        )
        node
        context


inspectSignature : Config context -> Node Signature -> context -> context
inspectSignature config (Node _ signature) context =
    inspectTypeAnnotation config signature.typeAnnotation context


inspectTypeAnnotation : Config context -> Node TypeAnnotation -> context -> context
inspectTypeAnnotation config (Node _ typeReference) context =
    case typeReference of
        Typed _ typeArgs ->
            List.foldl (inspectTypeAnnotation config) context typeArgs

        Tupled typeAnnotations ->
            List.foldl (inspectTypeAnnotation config) context typeAnnotations

        Record recordDefinition ->
            List.foldl
                (Node.value >> Tuple.second >> inspectTypeAnnotation config)
                context
                recordDefinition

        GenericRecord _ recordDefinition ->
            List.foldl
                (Node.value >> Tuple.second >> inspectTypeAnnotation config)
                context
                (Node.value recordDefinition)

        FunctionTypeAnnotation left right ->
            List.foldl (inspectTypeAnnotation config) context [ left, right ]

        Unit ->
            context

        GenericType _ ->
            context


inspectExpression : Config context -> Node Expression -> context -> context
inspectExpression config (Node _ expression) context =
    case expression of
        UnitExpr ->
            context

        FunctionOrValue _ _ ->
            context

        PrefixOperator _ ->
            context

        Operator _ ->
            context

        Hex _ ->
            context

        Integer _ ->
            context

        Floatable _ ->
            context

        Negation x ->
            inspectExpression config x context

        Literal _ ->
            context

        CharLiteral _ ->
            context

        RecordAccess ex1 _ ->
            inspectExpression config ex1 context

        RecordAccessFunction _ ->
            context

        GLSLExpression _ ->
            context

        Application expressionList ->
            List.foldl (inspectExpression config) context expressionList

        OperatorApplication _ _ left right ->
            List.foldl (inspectExpression config) context [ left, right ]

        IfBlock e1 e2 e3 ->
            List.foldl (inspectExpression config) context [ e1, e2, e3 ]

        TupledExpression expressionList ->
            List.foldl (inspectExpression config) context expressionList

        ParenthesizedExpression inner ->
            inspectExpression config inner context

        LetExpression letBlock ->
            (inspectLetDeclarations config letBlock.declarations >> inspectExpression config letBlock.expression)
                context

        CaseExpression caseBlock ->
            let
                context2 =
                    inspectExpression config caseBlock.expression context

                context3 =
                    List.foldl (\a b -> inspectCase config a b) context2 caseBlock.cases
            in
            context3

        LambdaExpression lambda ->
            ignoreSomething
                (inspectExpression config lambda.expression)
                lambda
                context

        ListExpr expressionList ->
            List.foldl (inspectExpression config) context expressionList

        RecordExpr expressionStringList ->
            List.foldl (\a b -> inspectExpression config (Tuple.second <| Node.value a) b) context expressionStringList

        RecordUpdateExpression name updates ->
            ignoreSomething
                (\c -> List.foldl (\a b -> inspectExpression config (Tuple.second <| Node.value a) b) c updates)
                ( name, updates )
                context


inspectCase : Config context -> Case -> context -> context
inspectCase config caze context =
    ignoreSomething
        (inspectExpression config (Tuple.second caze))
        caze
        context
