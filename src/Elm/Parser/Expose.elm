module Elm.Parser.Expose exposing (definitionExpose, exposable, exposeDefinition, exposingListInner, infixExpose, maybeExposeDefinition, typeExpose)

import Combine exposing ((*>), (<$), (<$>), (<*>), Parser, choice, maybe, or, parens, sepBy, string, succeed, while)
import Combine.Char exposing (char)
import Elm.Parser.Ranges exposing (ranged, withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (exposingToken, functionName, typeName)
import Elm.Parser.Util exposing (moreThanIndentWhitespace, trimmed)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(All, Explicit), TopLevelExpose(FunctionExpose, InfixExpose, TypeExpose, TypeOrAliasExpose), ValueConstructorExpose)
import Elm.Syntax.Ranged exposing (Ranged)


maybeExposeDefinition : Parser State a -> Parser State (Maybe (Exposing a))
maybeExposeDefinition p =
    choice [ Just <$> exposeDefinition p, succeed Nothing ]


exposeDefinition : Parser State a -> Parser State (Exposing a)
exposeDefinition p =
    moreThanIndentWhitespace *> exposingToken *> maybe moreThanIndentWhitespace *> exposeListWith p


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
        <*> (maybe moreThanIndentWhitespace *> (Just <$> exposeListWith valueConstructorExpose))


valueConstructorExpose : Parser State ValueConstructorExpose
valueConstructorExpose =
    ranged typeName


exposingListInner : Parser State b -> Parser State (Exposing b)
exposingListInner p =
    or (withRange (All <$ trimmed (string "..")))
        (Explicit <$> sepBy (char ',') (trimmed p))


exposeListWith : Parser State b -> Parser State (Exposing b)
exposeListWith p =
    parens (exposingListInner p)


definitionExpose : Parser State (Ranged TopLevelExpose)
definitionExpose =
    ranged <|
        or
            (FunctionExpose <$> functionName)
            (TypeOrAliasExpose <$> typeName)
