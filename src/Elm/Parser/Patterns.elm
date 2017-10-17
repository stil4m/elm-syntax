module Elm.Parser.Patterns exposing (declarablePattern, pattern)

import Combine exposing ((*>), (<$), (<$>), (<*), (<*>), (>>=), Parser, between, choice, lazy, many, maybe, or, parens, sepBy, sepBy1, string, succeed)
import Combine.Num
import Elm.Parser.Ranges exposing (withRange, withRangeCustomStart, withRangeTuple)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (asToken, characterLiteral, functionName, stringLiteral, typeName)
import Elm.Parser.Util exposing (asPointer, moreThanIndentWhitespace, trimmed)
import Elm.Syntax.Pattern exposing (Pattern(AllPattern, AsPattern, CharPattern, FloatPattern, IntPattern, ListPattern, NamedPattern, QualifiedNamePattern, RecordPattern, StringPattern, TuplePattern, UnConsPattern, UnitPattern, VarPattern), QualifiedNameRef)
import Elm.Syntax.Range exposing (Range)


type alias RangelessPattern =
    Range -> Pattern


declarablePattern : Parser State Pattern
declarablePattern =
    lazy (\() -> withRange declarablePatternRangeless)


pattern : Parser State Pattern
pattern =
    lazy
        (\() ->
            withRangeTuple
                (choice
                    [ declarablePatternRangeless
                    , variablePattern
                    , namedPattern
                    ]
                )
                >>= promoteToCompositePattern
        )


promoteToCompositePattern : ( Range, Pattern ) -> Parser State Pattern
promoteToCompositePattern ( range, x ) =
    or
        (withRangeCustomStart range
            (choice
                [ unConsPattern2 x
                , asPattern2 x
                ]
            )
        )
        (succeed x)


declarablePatternRangeless : Parser State RangelessPattern
declarablePatternRangeless =
    lazy (\() -> choice [ allPattern, tuplePattern, recordPattern ])


nonNamedPattern : Parser State RangelessPattern
nonNamedPattern =
    lazy
        (\() ->
            choice
                [ declarablePatternRangeless
                , asPattern
                , variablePattern
                ]
        )


nonAsPattern : Parser State RangelessPattern
nonAsPattern =
    lazy
        (\() ->
            choice
                [ declarablePatternRangeless
                , variablePattern
                , namedPattern
                ]
        )


variablePattern : Parser State RangelessPattern
variablePattern =
    lazy
        (\() ->
            choice [ allPattern, charPattern, stringPattern, floatPattern, intPattern, unitPattern, varPattern, listPattern ]
        )


listPattern : Parser State RangelessPattern
listPattern =
    lazy
        (\() ->
            between
                (string "[")
                (string "]")
                (ListPattern <$> sepBy (string ",") (trimmed pattern))
        )


unConsPattern2 : Pattern -> Parser State RangelessPattern
unConsPattern2 p =
    lazy
        (\() ->
            UnConsPattern p <$> (trimmed (string "::") *> pattern)
        )


charPattern : Parser State RangelessPattern
charPattern =
    lazy (\() -> CharPattern <$> characterLiteral)


stringPattern : Parser State RangelessPattern
stringPattern =
    lazy (\() -> StringPattern <$> stringLiteral)


intPattern : Parser State RangelessPattern
intPattern =
    lazy (\() -> IntPattern <$> Combine.Num.int)


floatPattern : Parser State RangelessPattern
floatPattern =
    lazy (\() -> FloatPattern <$> Combine.Num.float)


asPattern : Parser State RangelessPattern
asPattern =
    lazy
        (\() ->
            succeed AsPattern
                <*> (maybe moreThanIndentWhitespace *> withRange nonAsPattern)
                <*> (moreThanIndentWhitespace *> asToken *> moreThanIndentWhitespace *> asPointer functionName)
        )


asPattern2 : Pattern -> Parser State RangelessPattern
asPattern2 p =
    lazy
        (\() ->
            AsPattern p
                <$> (moreThanIndentWhitespace *> asToken *> moreThanIndentWhitespace *> asPointer functionName)
        )


tuplePattern : Parser State RangelessPattern
tuplePattern =
    lazy
        (\() ->
            TuplePattern <$> parens (sepBy1 (string ",") (trimmed pattern))
        )


recordPattern : Parser State RangelessPattern
recordPattern =
    lazy
        (\() ->
            RecordPattern
                <$> between
                        (string "{" *> maybe moreThanIndentWhitespace)
                        (maybe moreThanIndentWhitespace *> string "}")
                        (sepBy1 (string ",") (trimmed (asPointer functionName)))
        )


varPattern : Parser State RangelessPattern
varPattern =
    lazy (\() -> VarPattern <$> functionName)


qualifiedNameRef : Parser State QualifiedNameRef
qualifiedNameRef =
    succeed QualifiedNameRef
        <*> many (typeName <* string ".")
        <*> typeName


qualifiedNamePattern : Parser State RangelessPattern
qualifiedNamePattern =
    QualifiedNamePattern <$> qualifiedNameRef


namedPattern : Parser State RangelessPattern
namedPattern =
    lazy
        (\() ->
            succeed NamedPattern
                <*> qualifiedNameRef
                <*> many (moreThanIndentWhitespace *> withRange (or qualifiedNamePattern nonNamedPattern))
        )


allPattern : Parser State RangelessPattern
allPattern =
    lazy (\() -> AllPattern <$ string "_")


unitPattern : Parser State RangelessPattern
unitPattern =
    lazy (\() -> UnitPattern <$ string "()")
