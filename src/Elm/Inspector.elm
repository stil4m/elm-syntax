module Elm.Inspector exposing (Config, inspect)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
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


inspect : Config a -> File -> a -> a
inspect config file context =
    inspectDeclarations config file.declarations context


inspectDeclarations : Config context -> List (Node Declaration) -> context -> context
inspectDeclarations config declarations context =
    List.foldl (inspectDeclaration config) context declarations


inspectDeclaration : Config context -> Node Declaration -> context -> context
inspectDeclaration config (Node r declaration) context =
    case declaration of
        FunctionDeclaration function ->
            config.onFunction (Node r function) context

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
inspectTypeAlias config node context =
    config.onTypeAlias node context


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
