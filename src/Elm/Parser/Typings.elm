module Elm.Parser.Typings exposing (typeAlias, typeDeclaration)

import Combine exposing (Parser, many, maybe, sepBy, string, succeed)
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, typeName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


typeDeclaration : Parser State Type
typeDeclaration =
    succeed Type
        |> Combine.andMap (typePrefix |> Combine.continueWith typeName)
        |> Combine.andMap genericList
        |> Combine.andMap (Layout.around (string "=") |> Combine.continueWith valueConstructors)


valueConstructors : Parser State (List ValueConstructor)
valueConstructors =
    sepBy (string "|") (maybe Layout.layout |> Combine.continueWith valueConstructor |> Combine.ignore (maybe Layout.layout))


valueConstructor : Parser State ValueConstructor
valueConstructor =
    withRange
        (succeed ValueConstructor
            |> Combine.andMap typeName
            |> Combine.andMap (many (Layout.layout |> Combine.continueWith typeAnnotation))
        )


typeAlias : Parser State TypeAlias
typeAlias =
    succeed (TypeAlias Nothing)
        |> Combine.andMap (typeAliasPrefix |> Combine.continueWith typeName)
        |> Combine.andMap genericList
        |> Combine.andMap (Layout.around (string "=") |> Combine.continueWith typeAnnotation)


genericList : Parser State (List String)
genericList =
    many (Layout.layout |> Combine.continueWith functionName)


typePrefix : Parser State ()
typePrefix =
    string "type" |> Combine.continueWith Layout.layout


typeAliasPrefix : Parser State ()
typeAliasPrefix =
    typePrefix
        |> Combine.continueWith (string "alias")
        |> Combine.continueWith Layout.layout
