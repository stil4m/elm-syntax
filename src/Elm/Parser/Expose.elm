module Elm.Parser.Expose exposing (definitionExpose, exposable, exposeDefinition, exposingListInner, infixExpose, typeExpose)

import Combine exposing (Parser, choice, maybe, or, parens, sepBy, string, succeed, while)
import Combine.Char exposing (char)
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (ranged, withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (exposingToken, functionName, typeName)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..), ValueConstructorExpose)
import Elm.Syntax.Ranged exposing (Ranged)


exposeDefinition : Parser State a -> Parser State (Exposing a)
exposeDefinition p =
    Layout.layout
        |> Combine.continueWith exposingToken
        |> Combine.continueWith (maybe Layout.layout)
        |> Combine.continueWith (exposeListWith p)


exposable : Parser State (Ranged TopLevelExpose)
exposable =
    choice
        [ typeExpose
        , infixExpose
        , definitionExpose
        ]


infixExpose : Parser State (Ranged TopLevelExpose)
infixExpose =
    ranged (Combine.map InfixExpose (parens (while ((/=) ')'))))


typeExpose : Parser State (Ranged TopLevelExpose)
typeExpose =
    ranged (Combine.map TypeExpose exposedType)


exposedType : Parser State ExposedType
exposedType =
    succeed ExposedType
        |> Combine.andMap typeName
        |> Combine.andMap
            (maybe Layout.layout
                |> Combine.continueWith (Combine.map Just (exposeListWith valueConstructorExpose))
            )


valueConstructorExpose : Parser State ValueConstructorExpose
valueConstructorExpose =
    ranged typeName


exposingListInner : Parser State b -> Parser State (Exposing b)
exposingListInner p =
    or (withRange (succeed All |> Combine.ignore (Layout.maybeAroundBothSides (string ".."))))
        (Combine.map Explicit (sepBy (char ',') (Layout.maybeAroundBothSides p)))


exposeListWith : Parser State b -> Parser State (Exposing b)
exposeListWith p =
    parens (exposingListInner p)


definitionExpose : Parser State (Ranged TopLevelExpose)
definitionExpose =
    ranged <|
        or
            (Combine.map FunctionExpose functionName)
            (Combine.map TypeOrAliasExpose typeName)
