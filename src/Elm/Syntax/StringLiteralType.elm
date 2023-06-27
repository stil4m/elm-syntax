module Elm.Syntax.StringLiteralType exposing (StringLiteralType(..))

{-| Information about quotes.

@docs StringLiteralType

-}


{-| Indicates whether a string literal is single (`"abc"`) or triple-quoted (`"""abc"""`).
-}
type StringLiteralType
    = SingleQuote
    | TripleQuote
