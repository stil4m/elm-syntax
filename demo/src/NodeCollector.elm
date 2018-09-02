module NodeCollector exposing (collect)

import Elm.Inspector
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)


nodeTransformer : Elm.Inspector.Order Context (Node a)
nodeTransformer =
    Elm.Inspector.Post addNode


nodeTransformer2 =
    Elm.Inspector.Post (\( a, b ) c -> c |> addNode a |> addNode b)


type alias Context =
    List (Node String)


addNode : Node a -> Context -> Context
addNode n c =
    Node.map Debug.toString n :: c


onDestructuring : Node ( Node Pattern, Node Expression ) -> Context -> Context
onDestructuring ((Node _ ( a, b )) as c) context =
    context |> addNode c |> addNode a |> addNode b


onFile : File -> Context -> Context
onFile f c =
    c
        |> (\context -> List.foldl addNode context f.comments)
        |> addNode f.moduleDefinition


collect file =
    Elm.Inspector.inspect
        { onFile = Elm.Inspector.Post onFile
        , onImport = nodeTransformer
        , onFunction = nodeTransformer
        , onSignature = nodeTransformer
        , onPortDeclaration = nodeTransformer
        , onTypeAlias = nodeTransformer
        , onDestructuring = Elm.Inspector.Post onDestructuring
        , onInfixDeclaration = nodeTransformer
        , onExpression = nodeTransformer
        , onOperatorApplication = Elm.Inspector.Continue
        , onTypeAnnotation = nodeTransformer
        , onType = nodeTransformer
        , onLambda = Elm.Inspector.Continue
        , onLetBlock = Elm.Inspector.Continue
        , onCase = Elm.Inspector.Continue
        , onFunctionOrValue = Elm.Inspector.Continue
        , onRecordAccess = Elm.Inspector.Continue
        , onRecordUpdate = Elm.Inspector.Continue
        }
        file
        []
