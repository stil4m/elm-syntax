module Elm.Parser.Patterns exposing (declarablePattern, pattern)

import Combine exposing ((*>), (<$), (<$>), (<*), (<*>), (>>=), Parser, between, choice, lazy, many, maybe, or, parens, sepBy, sepBy1, string, succeed)
import Combine.Num
import Elm.Parser.Ranges exposing (ranged, rangedWithCustomStart)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (asToken, characterLiteral, functionName, stringLiteral, typeName)
import Elm.Parser.Util exposing (asPointer, moreThanIndentWhitespace, trimmed)
import Elm.Syntax.Pattern exposing (Pattern(AllPattern, AsPattern, CharPattern, FloatPattern, IntPattern, ListPattern, NamedPattern, QualifiedNamePattern, RecordPattern, StringPattern, TuplePattern, UnConsPattern, UnitPattern, VarPattern), QualifiedNameRef)
import Elm.Syntax.Ranged exposing (Ranged)


declarablePattern : Parser State (Ranged Pattern)
declarablePattern =
    lazy (\() -> ranged declarablePatternRangeless)


pattern : Parser State (Ranged Pattern)
pattern =
    lazy
        (\() ->
            ranged
                (choice
                    [ declarablePatternRangeless
                    , variablePattern
                    , namedPattern
                    ]
                )
                >>= promoteToCompositePattern
        )


promoteToCompositePattern : Ranged Pattern -> Parser State (Ranged Pattern)
promoteToCompositePattern (( range, _ ) as y) =
    or
        (rangedWithCustomStart range
            (choice
                [ unConsPattern2 y
                , asPattern2 y
                ]
            )
        )
        (succeed y)


declarablePatternRangeless : Parser State Pattern
declarablePatternRangeless =
    lazy (\() -> choice [ allPattern, tuplePattern, recordPattern ])


nonNamedPattern : Parser State Pattern
nonNamedPattern =
    lazy
        (\() ->
            choice
                [ declarablePatternRangeless
                , asPattern
                , variablePattern
                ]
        )


nonAsPattern : Parser State Pattern
nonAsPattern =
    lazy
        (\() ->
            choice
                [ declarablePatternRangeless
                , variablePattern
                , namedPattern
                ]
        )


variablePattern : Parser State Pattern
variablePattern =
    lazy
        (\() ->
            choice [ allPattern, charPattern, stringPattern, floatPattern, intPattern, unitPattern, varPattern, listPattern ]
        )


listPattern : Parser State Pattern
listPattern =
    lazy
        (\() ->
            between
                (string "[")
                (string "]")
                (ListPattern <$> sepBy (string ",") (trimmed pattern))
        )


unConsPattern2 : Ranged Pattern -> Parser State Pattern
unConsPattern2 p =
    lazy
        (\() ->
            UnConsPattern p <$> (trimmed (string "::") *> pattern)
        )


charPattern : Parser State Pattern
charPattern =
    lazy (\() -> CharPattern <$> characterLiteral)


stringPattern : Parser State Pattern
stringPattern =
    lazy (\() -> StringPattern <$> stringLiteral)


intPattern : Parser State Pattern
intPattern =
    lazy (\() -> IntPattern <$> Combine.Num.int)


floatPattern : Parser State Pattern
floatPattern =
    lazy (\() -> FloatPattern <$> Combine.Num.float)


asPattern : Parser State Pattern
asPattern =
    lazy
        (\() ->
            succeed AsPattern
                <*> (maybe moreThanIndentWhitespace *> ranged nonAsPattern)
                <*> (moreThanIndentWhitespace *> asToken *> moreThanIndentWhitespace *> asPointer functionName)
        )


asPattern2 : Ranged Pattern -> Parser State Pattern
asPattern2 p =
    lazy
        (\() ->
            AsPattern p
                <$> (moreThanIndentWhitespace *> asToken *> moreThanIndentWhitespace *> asPointer functionName)
        )


tuplePattern : Parser State Pattern
tuplePattern =
    lazy
        (\() ->
            TuplePattern <$> parens (sepBy1 (string ",") (trimmed pattern))
        )


recordPattern : Parser State Pattern
recordPattern =
    lazy
        (\() ->
            RecordPattern
                <$> between
                        (string "{" *> maybe moreThanIndentWhitespace)
                        (maybe moreThanIndentWhitespace *> string "}")
                        (sepBy1 (string ",") (trimmed (asPointer functionName)))
        )


varPattern : Parser State Pattern
varPattern =
    lazy (\() -> VarPattern <$> functionName)


qualifiedNameRef : Parser State QualifiedNameRef
qualifiedNameRef =
    succeed QualifiedNameRef
        <*> many (typeName <* string ".")
        <*> typeName


qualifiedNamePattern : Parser State Pattern
qualifiedNamePattern =
    QualifiedNamePattern <$> qualifiedNameRef


namedPattern : Parser State Pattern
namedPattern =
    lazy
        (\() ->
            succeed NamedPattern
                <*> qualifiedNameRef
                <*> many (moreThanIndentWhitespace *> ranged (or qualifiedNamePattern nonNamedPattern))
        )


allPattern : Parser State Pattern
allPattern =
    lazy (\() -> AllPattern <$ string "_")


unitPattern : Parser State Pattern
unitPattern =
    lazy (\() -> UnitPattern <$ string "()")
