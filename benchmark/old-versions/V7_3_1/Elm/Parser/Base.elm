module V7_3_1.Elm.Parser.Base exposing (moduleName, typeIndicator)

import V7_3_1.Combine as Combine exposing (Parser, sepBy1, string)
import V7_3_1.Elm.Parser.Tokens as Tokens
import V7_3_1.Elm.Syntax.ModuleName exposing (ModuleName)


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
