module Elm.Inspector exposing (Config, inspect)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


type alias Config context =
    { onFunction : Node Function -> context -> context
    , onTypeAlias : Node TypeAlias -> context -> context
    , onType : Node Type -> context -> context
    , onPortDeclaration : Node Signature -> context -> context
    }


inspect : Config a -> File -> a -> a
inspect config file context =
    List.foldl (inspectDeclaration config) context file.declarations


inspectDeclaration : Config context -> Node Declaration -> context -> context
inspectDeclaration config (Node r declaration) context =
    case declaration of
        FunctionDeclaration function ->
            config.onFunction (Node r function) context

        AliasDeclaration typeAlias ->
            config.onTypeAlias (Node r typeAlias) context

        CustomTypeDeclaration typeDecl ->
            config.onType (Node r typeDecl) context

        PortDeclaration _ ->
            context

        InfixDeclaration _ ->
            context

        Destructuring _ _ ->
            -- Will never happen. Will be removed in v8
            context
