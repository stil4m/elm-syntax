module Char.Extra exposing (isAlphaNumFast)


isAlphaNumFast : Char -> Bool
isAlphaNumFast char =
    -- Char.isAlphaNum does not reuse the same Char.toCode and is therefore slightly slower
    let
        charCode : Int
        charCode =
            char |> Char.toCode
    in
    charCodeIsLower charCode || charCodeIsUpper charCode || charCodeIsDigit charCode


charCodeIsLower : Int -> Bool
charCodeIsLower code =
    0x61 <= code && code <= 0x7A


charCodeIsUpper : Int -> Bool
charCodeIsUpper code =
    code <= 0x5A && 0x41 <= code


charCodeIsDigit : Int -> Bool
charCodeIsDigit code =
    code <= 0x39 && 0x30 <= code
