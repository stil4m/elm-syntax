module Elm.Parser.Typings exposing (typeAlias, typeDeclaration)

import Combine exposing ((*>), (<*), (<*>), Parser, many, maybe, sepBy, string, succeed)
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
        <*> (typePrefix *> typeName)
        <*> genericList
        <*> (Layout.around (string "=") *> valueConstructors)


valueConstructors : Parser State (List ValueConstructor)
valueConstructors =
    sepBy (string "|") (maybe Layout.layout *> valueConstructor <* maybe Layout.layout)


valueConstructor : Parser State ValueConstructor
valueConstructor =
    withRange
        (succeed ValueConstructor
            <*> typeName
            <*> many (Layout.layout *> typeAnnotation)
        )


typeAlias : Parser State TypeAlias
typeAlias =
    succeed (TypeAlias Nothing)
        <*> (typeAliasPrefix *> typeName)
        <*> genericList
        <*> (Layout.around (string "=") *> typeAnnotation)


genericList : Parser State (List String)
genericList =
    many (Layout.layout *> functionName)


typePrefix : Parser State ()
typePrefix =
    string "type" *> Layout.layout


typeAliasPrefix : Parser State ()
typeAliasPrefix =
    typePrefix *> string "alias" *> Layout.layout
