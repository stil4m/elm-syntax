module Elm.Inspector exposing (Config, Order(..), defaultConfig, inspect)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.DestructurePattern exposing (DestructurePattern)
import Elm.Syntax.Expression exposing (Case, Expression(..), Function, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix, InfixDirection)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Port exposing (Port)
import Elm.Syntax.Range exposing (Range)
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


type alias Config r context =
    { onFile : Order context (File r)
    , onImport : Order context (Node r (Import r))
    , onFunction : Order context (Node r (Function r))
    , onTypeAlias : Order context (Node r (TypeAlias r))
    , onType : Order context (Node r (Type r))
    , onPortDeclaration : Order context (Node r (Port r))
    , onInfixDeclaration : Order context (Node r (Infix r))
    , onDestructuring : Order context (Node r ( Node r (DestructurePattern r), Node r (Expression r) ))
    , onSignature : Order context (Node r (Signature r))
    , onExpression : Order context (Node r (Expression r))
    , onOperatorApplication :
        Order
            context
            { operator : String
            , direction : InfixDirection
            , left : Node r (Expression r)
            , right : Node r (Expression r)
            }
    , onTypeAnnotation : Order context (Node r (TypeAnnotation r))
    , onLambda : Order context (Lambda r)
    , onLetBlock : Order context (LetBlock r)
    , onCase : Order context (Case r)
    , onFunctionOrValue : Order context ( ModuleName, String )
    , onRecordAccess : Order context ( Node r (Expression r), Node r String )
    , onRecordUpdate : Order context ( Node r String, Node r (RecordSetter r), List (Node r (RecordSetter r)) )
    }


defaultConfig : Config r x
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


inspect : Config r a -> File r -> a -> a
inspect config file context =
    actionLambda config.onFile
        (inspectImports config file.imports >> inspectDeclarations config file.declarations)
        file
        context


inspectImports : Config r context -> List (Node r (Import r)) -> context -> context
inspectImports config imports context =
    List.foldl (inspectImport config) context imports


inspectImport : Config r context -> Node r (Import r) -> context -> context
inspectImport config imp context =
    actionLambda config.onImport
        identity
        imp
        context


inspectDeclarations : Config r context -> List (Node r (Declaration r)) -> context -> context
inspectDeclarations config declarations context =
    List.foldl (inspectDeclaration config) context declarations


inspectLetDeclarations : Config r context -> List (Node r (LetDeclaration r)) -> context -> context
inspectLetDeclarations config declarations context =
    List.foldl (inspectLetDeclaration config) context declarations


inspectLetDeclaration : Config r context -> Node r (LetDeclaration r) -> context -> context
inspectLetDeclaration config (Node range declaration) context =
    case declaration of
        LetFunction function ->
            inspectFunction config (Node range function) context

        LetDestructuring pattern expression ->
            inspectDestructuring config (Node range ( pattern, expression )) context


inspectDeclaration : Config r context -> Node r (Declaration r) -> context -> context
inspectDeclaration config (Node r declaration) context =
    case declaration of
        FunctionDeclaration function ->
            inspectFunction config (Node r function) context

        AliasDeclaration typeAlias ->
            inspectTypeAlias config (Node r typeAlias) context

        CustomTypeDeclaration typeDecl ->
            inspectType config (Node r typeDecl) context

        PortDeclaration p ->
            inspectPortDeclaration config (Node r p) context

        InfixDeclaration inf ->
            actionLambda
                config.onInfixDeclaration
                identity
                (Node r inf)
                context


inspectType : Config r context -> Node r (Type r) -> context -> context
inspectType config tipe context =
    actionLambda
        config.onType
        (inspectTypeInner config <| Node.value tipe)
        tipe
        context


inspectTypeInner : Config r context -> Type r -> context -> context
inspectTypeInner config typeDecl context =
    List.foldl (inspectValueConstructor config)
        context
        (typeDecl.firstConstructor :: typeDecl.restOfConstructors)


inspectValueConstructor : Config r context -> Node r (ValueConstructor r) -> context -> context
inspectValueConstructor config (Node _ valueConstructor) context =
    List.foldl (inspectTypeAnnotation config) context valueConstructor.arguments


inspectTypeAlias : Config r context -> Node r (TypeAlias r) -> context -> context
inspectTypeAlias config ((Node _ typeAlias) as pair) context =
    actionLambda
        config.onTypeAlias
        (inspectTypeAnnotation config typeAlias.typeAnnotation)
        pair
        context


inspectDestructuring : Config r context -> Node r ( Node r (DestructurePattern r), Node r (Expression r) ) -> context -> context
inspectDestructuring config destructuring context =
    actionLambda
        config.onDestructuring
        (\c ->
            c
                |> inspectExpression config (Tuple.second <| Node.value destructuring)
        )
        destructuring
        context


inspectFunction : Config r context -> Node r (Function r) -> context -> context
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


inspectPortDeclaration : Config r context -> Node r (Port r) -> context -> context
inspectPortDeclaration config node context =
    actionLambda
        config.onPortDeclaration
        (inspectSignature config (Node.value node).signature)
        node
        context


inspectSignature : Config r context -> Node r (Signature r) -> context -> context
inspectSignature config ((Node _ signature) as node) context =
    actionLambda
        config.onSignature
        (inspectTypeAnnotation config signature.typeAnnotation)
        node
        context


inspectTypeAnnotation : Config r context -> Node r (TypeAnnotation r) -> context -> context
inspectTypeAnnotation config typeAnnotation context =
    actionLambda
        config.onTypeAnnotation
        (inspectTypeAnnotationInner config typeAnnotation)
        typeAnnotation
        context


inspectTypeAnnotationInner : Config r context -> Node r (TypeAnnotation r) -> context -> context
inspectTypeAnnotationInner config (Node _ typeRefence) context =
    case typeRefence of
        Elm.Syntax.TypeAnnotation.Type _ typeArgs ->
            List.foldl (inspectTypeAnnotation config) context typeArgs

        Tuple typeAnnotations ->
            List.foldl (inspectTypeAnnotation config) context typeAnnotations

        Record recordDefinition ->
            List.foldl (inspectTypeAnnotation config) context (List.map (Node.value >> Tuple.second) recordDefinition)

        FunctionTypeAnnotation left right ->
            List.foldl (inspectTypeAnnotation config) context [ left, right ]

        ExtensionRecord _ firstField restOfFields ->
            List.foldl (inspectTypeAnnotation config) context (List.map (Node.value >> Tuple.second) (firstField :: restOfFields))

        Var _ ->
            context


inspectExpression : Config r context -> Node r (Expression r) -> context -> context
inspectExpression config ((Node _ expression) as node) context =
    actionLambda
        config.onExpression
        (inspectInnerExpression config expression)
        node
        context


inspectInnerExpression : Config r context -> Expression r -> context -> context
inspectInnerExpression config expression context =
    case expression of
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

        Application head expressionList ->
            List.foldl (inspectExpression config) context (head :: expressionList)

        OperatorApplication op dir left right ->
            actionLambda config.onOperatorApplication
                (\base -> List.foldl (inspectExpression config) base [ left, right ])
                { operator = op, direction = dir, left = left, right = right }
                context

        IfBlock e1 e2 e3 ->
            List.foldl (inspectExpression config) context [ e1, e2, e3 ]

        TupleExpression expressionList ->
            List.foldl (inspectExpression config) context expressionList

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
                    List.foldl (\a b -> inspectCase config a b) context2 (caseBlock.firstCase :: caseBlock.restOfCases)
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

        RecordUpdateExpression name firstUpdate updates ->
            actionLambda config.onRecordUpdate
                (\c -> List.foldl (\a b -> inspectExpression config (Tuple.second <| Node.value a) b) c (firstUpdate :: updates))
                ( name, firstUpdate, updates )
                context


inspectCase : Config r context -> Case r -> context -> context
inspectCase config caze context =
    actionLambda config.onCase
        (inspectExpression config (Tuple.second caze))
        caze
        context
