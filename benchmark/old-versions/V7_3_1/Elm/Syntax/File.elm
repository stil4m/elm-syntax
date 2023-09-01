module V7_3_1.Elm.Syntax.File exposing
    ( File
    , encode, decoder
    )

{-| This syntax represents a whole Elm file.


## Types

@docs File


## Serialization

@docs encode, decoder

-}

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import V7_3_1.Elm.Syntax.Comments as Comments exposing (Comment)
import V7_3_1.Elm.Syntax.Declaration as Declaration exposing (Declaration)
import V7_3_1.Elm.Syntax.Import as Import exposing (Import)
import V7_3_1.Elm.Syntax.Module as Module exposing (Module)
import V7_3_1.Elm.Syntax.Node as Node exposing (Node)


{-| Type annotation for a file
-}
type alias File =
    { moduleDefinition : Node Module
    , imports : List (Node Import)
    , declarations : List (Node Declaration)
    , comments : List (Node Comment)
    }


{-| Encode a `File` syntax element to JSON.
-}
encode : File -> Value
encode { moduleDefinition, imports, declarations, comments } =
    JE.object
        [ ( "moduleDefinition", Node.encode Module.encode moduleDefinition )
        , ( "imports", JE.list (Node.encode Import.encode) imports )
        , ( "declarations", JE.list (Node.encode Declaration.encode) declarations )
        , ( "comments", JE.list (Node.encode Comments.encode) comments )
        ]


{-| JSON decoder for a `File` syntax element.
-}
decoder : Decoder File
decoder =
    JD.map4 File
        (JD.field "moduleDefinition" (Node.decoder Module.decoder))
        (JD.field "imports" (JD.list (Node.decoder Import.decoder)))
        (JD.field "declarations" (JD.list (Node.decoder Declaration.decoder)))
        (JD.field "comments" (JD.list (Node.decoder Comments.decoder)))
