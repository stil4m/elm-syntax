module Elm.Parser.Expose exposing (definitionExpose, exposable, exposeDefinition, exposingListInner, infixExpose, typeExpose)

import Combine exposing ((*>), (<$), (<$>), (<*>), Parser, choice, maybe, or, parens, sepBy, string, succeed, while)
import Combine.Char exposing (char)
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (ranged, withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (exposingToken, functionName, typeName)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(All, Explicit), TopLevelExpose(FunctionExpose, InfixExpose, TypeExpose, TypeOrAliasExpose), ValueConstructorExpose)
import Elm.Syntax.Ranged exposing (Ranged)


exposeDefinition : Parser State a -> Parser State (Exposing a)
exposeDefinition p =
    Layout.layout *> exposingToken *> maybe Layout.layout *> exposeListWith p


exposable : Parser State (Ranged TopLevelExpose)
exposable =
    choice
        [ typeExpose
        , infixExpose
        , definitionExpose
        ]


infixExpose : Parser State (Ranged TopLevelExpose)
infixExpose =
    ranged (InfixExpose <$> parens (while ((/=) ')')))


typeExpose : Parser State (Ranged TopLevelExpose)
typeExpose =
    ranged (TypeExpose <$> exposedType)


exposedType : Parser State ExposedType
exposedType =
    succeed ExposedType
        <*> typeName
        <*> (maybe Layout.layout *> (Just <$> exposeListWith valueConstructorExpose))


valueConstructorExpose : Parser State ValueConstructorExpose
valueConstructorExpose =
    ranged typeName


exposingListInner : Parser State b -> Parser State (Exposing b)
exposingListInner p =
    or (withRange (All <$ Layout.maybeAroundBothSides (string "..")))
        (Explicit <$> sepBy (char ',') (Layout.maybeAroundBothSides p))


exposeListWith : Parser State b -> Parser State (Exposing b)
exposeListWith p =
    parens (exposingListInner p)


definitionExpose : Parser State (Ranged TopLevelExpose)
definitionExpose =
    ranged <|
        or
            (FunctionExpose <$> functionName)
            (TypeOrAliasExpose <$> typeName)
