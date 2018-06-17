module Elm.Parser.Base exposing (moduleName, typeIndicator, variablePointer)

import Combine exposing ((<$>), Parser, sepBy1, string)
import Combine.Extra as Combine
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Base exposing (ModuleName, VariablePointer)


variablePointer : Parser State String -> Parser State VariablePointer
variablePointer p =
    withRange (VariablePointer <$> p)


moduleName : Parser s ModuleName
moduleName =
    sepBy1 (string ".") Tokens.typeName


typeIndicator : Parser s ( ModuleName, String )
typeIndicator =
    let
        helper ( n, xs ) =
            Combine.choice
                [ string "."
                    |> Combine.continueWith Tokens.typeName
                    |> Combine.andThen (\t -> helper ( t, n :: xs ))
                , Combine.succeed ( n, xs )
                ]
    in
    Tokens.typeName
        |> Combine.andThen (\t -> helper ( t, [] ))
        |> Combine.map (\( t, xs ) -> ( List.reverse xs, t ))
