module Elm.Syntax.Declaration exposing
    ( Declaration(..)
    , encode, decoder
    )

{-| Syntax for the different top-level declarations in Elm.
These can be one of the following (all declared in `Declaration`):

  - Functions: `add x y = x + y`
  - Custom types: `type Color = Blue | Red`
  - Type aliases: `type alias Status = Int`
  - Port declaration: `port sendMessage: String -> Cmd msg`
  - Destructuring: `{name, age} = person`
  - Infix declarations. You will probably not need this, while only core packages can define these.


## Types

@docs Declaration


## Serialization

@docs encode, decoder

-}

import Elm.Json.Util exposing (decodeTyped, encodeTyped)
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Infix as Infix exposing (Infix)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Signature as Signature exposing (Signature)
import Elm.Syntax.Type as Type exposing (Type)
import Elm.Syntax.TypeAlias as TypeAlias exposing (TypeAlias)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Custom type that represents all different top-level declarations.
-}
type Declaration
    = FunctionDeclaration Function
    | AliasDeclaration TypeAlias
    | CustomTypeDeclaration Type
    | PortDeclaration Signature
    | InfixDeclaration Infix
    | Destructuring (Node Pattern) (Node Expression)



-- Serialization


{-| Encode a `Declaration` syntax element to JSON.
-}
encode : Declaration -> Value
encode decl =
    case decl of
        FunctionDeclaration function ->
            encodeTyped "function" (Expression.encodeFunction function)

        AliasDeclaration typeAlias ->
            encodeTyped "typeAlias" (TypeAlias.encode typeAlias)

        CustomTypeDeclaration typeDeclaration ->
            encodeTyped "typedecl" (Type.encode typeDeclaration)

        PortDeclaration sig ->
            encodeTyped "port" (Signature.encode sig)

        InfixDeclaration inf ->
            encodeTyped "infix"
                (Infix.encode inf)

        Destructuring pattern expression ->
            encodeTyped "destructuring"
                (JE.object
                    [ ( "pattern", Node.encode Pattern.encode pattern )
                    , ( "expression", Node.encode Expression.encode expression )
                    ]
                )


{-| JSON decoder for a `Declaration` syntax element.
-}
decoder : Decoder Declaration
decoder =
    JD.lazy
        (\() ->
            decodeTyped
                [ ( "function", Expression.functionDecoder |> JD.map FunctionDeclaration )
                , ( "typeAlias", TypeAlias.decoder |> JD.map AliasDeclaration )
                , ( "typedecl", Type.decoder |> JD.map CustomTypeDeclaration )
                , ( "port", Signature.decoder |> JD.map PortDeclaration )
                , ( "infix", Infix.decoder |> JD.map InfixDeclaration )
                , ( "destructuring", JD.map2 Destructuring (JD.field "pattern" (Node.decoder Pattern.decoder)) (JD.field "expression" (Node.decoder Expression.decoder)) )
                ]
        )
