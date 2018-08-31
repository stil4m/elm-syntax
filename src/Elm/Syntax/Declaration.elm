module Elm.Syntax.Declaration exposing
    ( Declaration(..)
    , encode, decoder
    )

{-| Declarations Syntax


# Types

@docs Declaration


# Serialization

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


{-| Union type that represents all different top level declarations.
-}
type Declaration
    = FunctionDeclaration Function
    | AliasDeclaration TypeAlias
    | CustomTypeDeclaration Type
    | PortDeclaration Signature
    | InfixDeclaration Infix
    | Destructuring (Node Pattern) (Node Expression)



-- Serialization


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


decoder : Decoder Declaration
decoder =
    JD.lazy
        (\() ->
            decodeTyped
                [ ( "function", Expression.functionDecoder |> JD.map FunctionDeclaration )
                , ( "typeAlias", TypeAlias.decoder |> JD.map AliasDeclaration )
                , ( "typedecl", Type.decoder |> JD.map CustomTypeDeclaration )
                , ( "port", Signature.decoder |> JD.map PortDeclaration )
                , ( "infix", Infix.decode |> JD.map InfixDeclaration )
                , ( "destructuring", JD.map2 Destructuring (JD.field "pattern" (Node.decoder Pattern.decoder)) (JD.field "expression" (Node.decoder Expression.decoder)) )
                ]
        )
