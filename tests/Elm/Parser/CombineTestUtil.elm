module Elm.Parser.CombineTestUtil exposing (parseAsFarAsPossibleWithState, parseFullString, parseFullStringState, parseFullStringWithNullState, parseStateToMaybe, pushIndent)

import Combine exposing (..)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (..)


pushIndent : Int -> Parser State b -> Parser State b
pushIndent x p =
    modifyState (Elm.Parser.State.pushColumn (x + 1)) |> Combine.continueWith p


parseFullStringState : State -> String -> Parser State b -> Maybe b
parseFullStringState state s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) state s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseStateToMaybe : State -> String -> Parser State b -> Maybe ( b, State )
parseStateToMaybe state s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) state s of
        Ok ( x, r ) ->
            Just ( r, x )

        _ ->
            Nothing


parseFullStringWithNullState : String -> Parser State b -> Maybe b
parseFullStringWithNullState s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) emptyState s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseFullString : String -> Parser () b -> Maybe b
parseFullString s p =
    case Combine.parse (p |> Combine.ignore Combine.end) s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseAsFarAsPossibleWithState : State -> String -> Parser State b -> Maybe b
parseAsFarAsPossibleWithState state s p =
    case Combine.runParser p state s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


unRange : Node a -> Node a
unRange n =
    unRanged identity n


unRanged : (a -> a) -> Node a -> Node a
unRanged f (Node _ a) =
    Node.empty <| f a


noRangeRecordField : RecordField -> RecordField
noRangeRecordField ( a, b ) =
    ( unRange a, noRangeTypeReference b )


noRangeRecordDefinition : RecordDefinition -> RecordDefinition
noRangeRecordDefinition =
    List.map (unRanged noRangeRecordField)


noRangeTypeReference : Node TypeAnnotation -> Node TypeAnnotation
noRangeTypeReference (Node _ typeAnnotation) =
    Node.empty <|
        case typeAnnotation of
            GenericType x ->
                GenericType x

            Typed (Node _ ( a, b )) c ->
                Typed (Node.empty ( a, b )) (List.map noRangeTypeReference c)

            Unit ->
                Unit

            Tupled a ->
                Tupled (List.map noRangeTypeReference a)

            Record a ->
                Record (List.map (unRanged noRangeRecordField) a)

            GenericRecord a b ->
                GenericRecord (unRanged identity a) (unRanged noRangeRecordDefinition b)

            FunctionTypeAnnotation a b ->
                FunctionTypeAnnotation
                    (noRangeTypeReference a)
                    (noRangeTypeReference b)
