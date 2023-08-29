module Elm.Parser.Base exposing (moduleName, typeIndicator)

import Combine exposing (Parser, sepBy1, string)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)


moduleName : Parser s ModuleName
moduleName =
    sepBy1 (string ".") Tokens.typeName


typeIndicator : Parser s ( ModuleName, String )
typeIndicator =
    let
        helper : ( ModuleName, String ) -> Parser s ( ModuleName, String )
        helper (( xs, n ) as acc) =
            Combine.oneOf
                [ string "."
                    |> Combine.continueWith Tokens.typeName
                    |> Combine.andThen (\t -> helper ( n :: xs, t ))
                , Combine.succeed acc
                ]
    in
    Tokens.typeName
        |> Combine.andThen (\t -> helper ( [], t ))
        |> Combine.map (\( xs, t ) -> ( List.reverse xs, t ))
