module Elm.Inspector exposing (Config, Order(..), defaultConfig, inspect)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, Expression(..), Function, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix, InfixDirection)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))


type Order context x
    = Skip
    | Continue
    | Pre (x -> context -> context)
    | Post (x -> context -> context)
    | Inner ((context -> context) -> x -> context -> context)


type alias Config context =
    { onFile : Order context File
    , onImport : Order context (Node Import)
    , onFunction : Order context (Node Function)
    , onTypeAlias : Order context (Node TypeAlias)
    , onType : Order context (Node Type)
    , onPortDeclaration : Order context (Node Signature)
    , onInfixDeclaration : Order context (Node Infix)
    , onDestructuring : Order context (Node ( Node Pattern, Node Expression ))
    , onSignature : Order context (Node Signature)
    , onExpression : Order context (Node Expression)
    , onOperatorApplication :
        Order context
            { operator : String
            , direction : InfixDirection
            , left : Node Expression
            , right : Node Expression
            }
    , onTypeAnnotation : Order context (Node TypeAnnotation)
    , onLambda : Order context Lambda
    , onLetBlock : Order context LetBlock
    , onCase : Order context Case
    , onFunctionOrValue : Order context ( ModuleName, String )
    , onRecordAccess : Order context ( Node Expression, Node String )
    , onRecordUpdate : Order context ( Node String, List (Node RecordSetter) )
    }


defaultConfig : Config x
defaultConfig =
    { onFile = Continue
    , onImport = Continue
    , onFunction = Continue
    , onPortDeclaration = Continue
    , onInfixDeclaration = Continue
    , onSignature = Continue
    , onTypeAnnotation = Continue
    , onType = Continue
    , onTypeAlias = Continue
    , onDestructuring = Continue
    , onExpression = Continue
    , onLambda = Continue
    , onOperatorApplication = Continue
    , onLetBlock = Continue
    , onCase = Continue
    , onFunctionOrValue = Continue
    , onRecordAccess = Continue
    , onRecordUpdate = Continue
    }


actionLambda : Order config x -> (config -> config) -> x -> config -> config
actionLambda act =
    case act of
        Skip ->
            \_ _ c -> c

        Continue ->
            \f _ c -> f c

        Pre g ->
            \f x c -> g x c |> f

        Post g ->
            \f x c -> f c |> g x

        Inner g ->
            \f x c -> g f x c


inspect : Config a -> File -> a -> a
inspect config file context =
    actionLambda config.onFile
        (inspectImports config file.imports >> inspectDeclarations config file.declarations)
        file
        context


inspectImports : Config context -> List (Node Import) -> context -> context
inspectImports config imports context =
    List.foldl (inspectImport config) context imports


inspectImport : Config context -> Node Import -> context -> context
inspectImport config imp context =
    actionLambda config.onImport
        identity
        imp
        context


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
            inspectPortDeclaration config (Node r signature) context

        InfixDeclaration inf ->
            actionLambda
                config.onInfixDeclaration
                identity
                (Node r inf)
                context

        Destructuring pattern expresion ->
            inspectDestructuring config (Node r ( pattern, expresion )) context


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
    actionLambda
        config.onDestructuring
        (\c ->
            c
                |> inspectExpression config (Tuple.second <| Node.value destructuring)
        )
        destructuring
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


inspectPortDeclaration : Config context -> Node Signature -> context -> context
inspectPortDeclaration config signature context =
    actionLambda
        config.onPortDeclaration
        (inspectSignature config signature)
        signature
        context


inspectSignature : Config context -> Node Signature -> context -> context
inspectSignature config ((Node _ signature) as node) context =
    actionLambda
        config.onSignature
        (inspectTypeAnnotation config signature.typeAnnotation)
        node
        context


inspectTypeAnnotation : Config context -> Node TypeAnnotation -> context -> context
inspectTypeAnnotation config typeAnnotation context =
    actionLambda
        config.onTypeAnnotation
        (inspectTypeAnnotationInner config typeAnnotation)
        typeAnnotation
        context


inspectTypeAnnotationInner : Config context -> Node TypeAnnotation -> context -> context
inspectTypeAnnotationInner config (Node _ typeRefence) context =
    case typeRefence of
        Typed _ typeArgs ->
            List.foldl (inspectTypeAnnotation config) context typeArgs

        Tupled typeAnnotations ->
            List.foldl (inspectTypeAnnotation config) context typeAnnotations

        Record recordDefinition ->
            List.foldl (inspectTypeAnnotation config) context (List.map (Node.value >> Tuple.second) recordDefinition)

        GenericRecord _ recordDefinition ->
            List.foldl (inspectTypeAnnotation config) context (List.map (Node.value >> Tuple.second) <| Node.value recordDefinition)

        FunctionTypeAnnotation left right ->
            List.foldl (inspectTypeAnnotation config) context [ left, right ]

        Unit ->
            context

        GenericType _ ->
            context


inspectExpression : Config context -> Node Expression -> context -> context
inspectExpression config ((Node _ expression) as node) context =
    actionLambda
        config.onExpression
        (inspectInnerExpression config expression)
        node
        context


inspectInnerExpression : Config context -> Expression -> context -> context
inspectInnerExpression config expression context =
    case expression of
        UnitExpr ->
            context

        FunctionOrValue moduleName functionOrVal ->
            actionLambda config.onFunctionOrValue
                identity
                ( moduleName, functionOrVal )
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

        RecordAccess ex1 key ->
            actionLambda config.onRecordAccess
                (inspectExpression config ex1)
                ( ex1, key )
                context

        RecordAccessFunction _ ->
            context

        GLSLExpression _ ->
            context

        Application expressionList ->
            List.foldl (inspectExpression config) context expressionList

        OperatorApplication op dir left right ->
            actionLambda config.onOperatorApplication
                (\base -> List.foldl (inspectExpression config) base [ left, right ])
                { operator = op, direction = dir, left = left, right = right }
                context

        IfBlock e1 e2 e3 ->
            List.foldl (inspectExpression config) context [ e1, e2, e3 ]

        TupledExpression expressionList ->
            List.foldl (inspectExpression config) context expressionList

        ParenthesizedExpression inner ->
            inspectExpression config inner context

        LetExpression letBlock ->
            let
                next =
                    inspectLetDeclarations config letBlock.declarations >> inspectExpression config letBlock.expression
            in
            actionLambda config.onLetBlock
                next
                letBlock
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
            actionLambda config.onLambda
                (inspectExpression config lambda.expression)
                lambda
                context

        ListExpr expressionList ->
            List.foldl (inspectExpression config) context expressionList

        RecordExpr expressionStringList ->
            List.foldl (\a b -> inspectExpression config (Tuple.second <| Node.value a) b) context expressionStringList

        RecordUpdateExpression name updates ->
            actionLambda config.onRecordUpdate
                (\c -> List.foldl (\a b -> inspectExpression config (Tuple.second <| Node.value a) b) c updates)
                ( name, updates )
                context


inspectCase : Config context -> Case -> context -> context
inspectCase config caze context =
    actionLambda config.onCase
        (inspectExpression config (Tuple.second caze))
        caze
        context
