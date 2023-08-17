module Elm.Parser.Signature exposing
    ( functionSignature
    , functionSignatureFromVarPointer
    )

import Combine exposing (Parser, maybe, string, succeed)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Signature exposing (Signature)


functionSignature : Parser State (Node Signature)
functionSignature =
    Node.parser functionName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen functionSignatureFromVarPointer


functionSignatureFromVarPointer : Node String -> Parser State (Node Signature)
functionSignatureFromVarPointer varPointer =
    succeed (\ta -> Node.combine Signature varPointer ta)
        |> Combine.ignore (string ":")
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andMap typeAnnotation
