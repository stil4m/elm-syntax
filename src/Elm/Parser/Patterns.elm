module Elm.Parser.Patterns exposing (declarablePattern, pattern)

import Combine exposing (Parser, between, choice, lazy, many, maybe, or, parens, sepBy, sepBy1, string, succeed)
import Combine.Num
import Elm.Parser.Base exposing (variablePointer)
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (ranged, rangedWithCustomStart)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (asToken, characterLiteral, functionName, stringLiteral, typeName)
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
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
                |> Combine.andThen promoteToCompositePattern
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
                (Combine.map ListPattern (sepBy (string ",") (Layout.maybeAroundBothSides pattern)))
        )


unConsPattern2 : Ranged Pattern -> Parser State Pattern
unConsPattern2 p =
    lazy
        (\() ->
            Combine.map (UnConsPattern p) (Layout.maybeAroundBothSides (string "::") |> Combine.continueWith pattern)
        )


charPattern : Parser State Pattern
charPattern =
    lazy (\() -> Combine.map CharPattern characterLiteral)


stringPattern : Parser State Pattern
stringPattern =
    lazy (\() -> Combine.map StringPattern stringLiteral)


intPattern : Parser State Pattern
intPattern =
    lazy (\() -> Combine.map IntPattern Combine.Num.int)


floatPattern : Parser State Pattern
floatPattern =
    lazy (\() -> Combine.map FloatPattern Combine.Num.float)


asPattern : Parser State Pattern
asPattern =
    lazy
        (\() ->
            succeed AsPattern
                |> Combine.andMap (maybe Layout.layout |> Combine.continueWith (ranged nonAsPattern))
                |> Combine.andMap
                    (Layout.layout
                        |> Combine.continueWith asToken
                        |> Combine.continueWith Layout.layout
                        |> Combine.continueWith (variablePointer functionName)
                    )
        )


asPattern2 : Ranged Pattern -> Parser State Pattern
asPattern2 p =
    lazy
        (\() ->
            Combine.map
                (AsPattern p)
                (Layout.layout
                    |> Combine.continueWith asToken
                    |> Combine.continueWith Layout.layout
                    |> Combine.continueWith (variablePointer functionName)
                )
        )


tuplePattern : Parser State Pattern
tuplePattern =
    lazy
        (\() ->
            Combine.map
                TuplePattern
                (parens (sepBy1 (string ",") (Layout.maybeAroundBothSides pattern)))
        )


recordPattern : Parser State Pattern
recordPattern =
    lazy
        (\() ->
            Combine.map RecordPattern
                (between
                    (string "{" |> Combine.continueWith (maybe Layout.layout))
                    (maybe Layout.layout |> Combine.continueWith (string "}"))
                    (sepBy1 (string ",") (Layout.maybeAroundBothSides (variablePointer functionName)))
                )
        )


varPattern : Parser State Pattern
varPattern =
    lazy (\() -> Combine.map VarPattern functionName)


qualifiedNameRef : Parser State QualifiedNameRef
qualifiedNameRef =
    succeed QualifiedNameRef
        |> Combine.andMap (many (typeName |> Combine.ignore (string ".")))
        |> Combine.andMap typeName


qualifiedNamePattern : Parser State Pattern
qualifiedNamePattern =
    Combine.map QualifiedNamePattern qualifiedNameRef


namedPattern : Parser State Pattern
namedPattern =
    lazy
        (\() ->
            succeed NamedPattern
                |> Combine.andMap qualifiedNameRef
                |> Combine.andMap (many (Layout.layout |> Combine.continueWith (ranged (or qualifiedNamePattern nonNamedPattern))))
        )


allPattern : Parser State Pattern
allPattern =
    lazy (\() -> Combine.map (always AllPattern) (string "_"))


unitPattern : Parser State Pattern
unitPattern =
    lazy (\() -> Combine.map (always UnitPattern) (string "()"))
