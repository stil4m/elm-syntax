module Char.Extra exposing (isLatinAlphaNumOrUnderscoreFast, unicodeIsAlphaNumOrUnderscoreFast, unicodeIsLowerFast, unicodeIsUpperFast)

{-| Edited from [minibill/elm-unicode](https://package.elm-lang.org/packages/miniBill/elm-unicode/latest/)

Copyright 2021 Leonardo Taglialegne

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1.  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2.  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3.  Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

--


unicodeIsLowerFast : Char -> Bool
unicodeIsLowerFast c =
    let
        code : Int
        code =
            Char.toCode c

        cString : String
        cString =
            String.fromChar c
    in
    charCodeIsLower code
        || (if String.toLower cString == cString ++ "" && String.toUpper cString /= cString ++ "" then
                code <= 0x0344 || 0x0346 <= code && code <= 0x216F || 0x2180 <= code && code <= 0x24CF || 0x24EA <= code && code <= 0x000F0000

            else if code < 0xA7F9 then
                if code < 0x2109 then
                    if code < 0x024E then
                        0x0137 <= code && code <= 0x0138 || 0x018C <= code && code <= 0x018D || 0x0199 <= code && code <= 0x019B || 0x01AA <= code && code <= 0x01AB || 0x01B9 <= code && code <= 0x01BA || 0x01BD <= code && code <= 0x01BF || code == 0x0221 || 0x0233 <= code && code <= 0x0239

                    else
                        0x024F <= code && code <= 0x0293 || 0x0295 <= code && code <= 0x02AF || 0x03FB <= code && code <= 0x03FC || 0x0560 <= code && code <= 0x0588 || 0x1D00 <= code && code <= 0x1D2B || 0x1D6B <= code && code <= 0x1D77 || 0x1D79 <= code && code <= 0x1D9A || 0x1E95 <= code && code <= 0x1E9D || code == 0x1E9F

                else if code < 0x2C70 then
                    code == 0x210A || 0x210E <= code && code <= 0x210F || code == 0x2113 || code == 0x212F || code == 0x2134 || code == 0x2139 || 0x213C <= code && code <= 0x213D || 0x2146 <= code && code <= 0x2149

                else
                    code == 0x2C71 || 0x2C73 <= code && code <= 0x2C74 || 0x2C76 <= code && code <= 0x2C7B || 0x2CE3 <= code && code <= 0x2CE4 || 0xA72F <= code && code <= 0xA731 || 0xA771 <= code && code <= 0xA778 || code == 0xA78E || 0xA793 <= code && code <= 0xA795 || code == 0xA7AF || modBy 2 code == 1 && 0xA7D3 <= code && code <= 0xA7D5

            else if code < 0x0001D621 then
                if code < 0x0001D4BA then
                    code == 0xA7FA || 0xAB30 <= code && code <= 0xAB5A || 0xAB60 <= code && code <= 0xAB68 || 0x0001D41A <= code && code <= 0x0001D433 || 0x0001D44E <= code && code <= 0x0001D454 || 0x0001D456 <= code && code <= 0x0001D467 || 0x0001D482 <= code && code <= 0x0001D49B || 0x0001D4B6 <= code && code <= 0x0001D4B9

                else
                    code == 0x0001D4BB || 0x0001D4BD <= code && code <= 0x0001D4C3 || 0x0001D4C5 <= code && code <= 0x0001D4CF || 0x0001D4EA <= code && code <= 0x0001D503 || 0x0001D51E <= code && code <= 0x0001D537 || 0x0001D552 <= code && code <= 0x0001D56B || 0x0001D586 <= code && code <= 0x0001D59F || 0x0001D5BA <= code && code <= 0x0001D5D3 || 0x0001D5EE <= code && code <= 0x0001D607

            else if code < 0x0001D74F then
                0x0001D622 <= code && code <= 0x0001D63B || 0x0001D656 <= code && code <= 0x0001D66F || 0x0001D68A <= code && code <= 0x0001D6A5 || 0x0001D6C2 <= code && code <= 0x0001D6DA || 0x0001D6DC <= code && code <= 0x0001D6E1 || 0x0001D6FC <= code && code <= 0x0001D714 || 0x0001D716 <= code && code <= 0x0001D71B || 0x0001D736 <= code && code <= 0x0001D74E

            else
                0x0001D750 <= code && code <= 0x0001D755 || 0x0001D770 <= code && code <= 0x0001D788 || 0x0001D78A <= code && code <= 0x0001D78F || 0x0001D7AA <= code && code <= 0x0001D7C2 || 0x0001D7C4 <= code && code <= 0x0001D7C9 || code == 0x0001D7CB || 0x0001DF00 <= code && code <= 0x0001DF09 || 0x0001DF0B <= code && code <= 0x0001DF1E || 0x0001DF25 <= code && code <= 0x0001DF2A
           )


unicodeIsUpperFast : Char -> Bool
unicodeIsUpperFast c =
    let
        code : Int
        code =
            Char.toCode c
    in
    charCodeIsUpper code
        || (let
                cString : String
                cString =
                    String.fromChar c
            in
            if String.toUpper cString == cString ++ "" && String.toLower cString /= cString ++ "" then
                code <= 0x215F || 0x2170 <= code && code <= 0x24B5 || 0x24D0 <= code && code <= 0x000F0000

            else if code < 0x0001D4CF then
                if code < 0x213D then
                    0x03D2 <= code && code <= 0x03D4 || code == 0x2102 || code == 0x2107 || 0x210B <= code && code <= 0x210D || 0x2110 <= code && code <= 0x2112 || code == 0x2115 || 0x2119 <= code && code <= 0x211D || code == 0x2124 || code == 0x2128 || 0x212A <= code && code <= 0x212D || 0x2130 <= code && code <= 0x2133

                else
                    0x213E <= code && code <= 0x213F || code == 0x2145 || 0x0001D400 <= code && code <= 0x0001D419 || 0x0001D434 <= code && code <= 0x0001D44D || 0x0001D468 <= code && code <= 0x0001D481 || code == 0x0001D49C || 0x0001D49E <= code && code <= 0x0001D49F || code == 0x0001D4A2 || 0x0001D4A5 <= code && code <= 0x0001D4A6 || 0x0001D4A9 <= code && code <= 0x0001D4AC || 0x0001D4AE <= code && code <= 0x0001D4B5

            else if code < 0x0001D59F then
                0x0001D4D0 <= code && code <= 0x0001D4E9 || 0x0001D504 <= code && code <= 0x0001D505 || 0x0001D507 <= code && code <= 0x0001D50A || 0x0001D50D <= code && code <= 0x0001D514 || 0x0001D516 <= code && code <= 0x0001D51C || 0x0001D538 <= code && code <= 0x0001D539 || 0x0001D53B <= code && code <= 0x0001D53E || 0x0001D540 <= code && code <= 0x0001D544 || code == 0x0001D546 || 0x0001D54A <= code && code <= 0x0001D550 || 0x0001D56C <= code && code <= 0x0001D585

            else
                0x0001D5A0 <= code && code <= 0x0001D5B9 || 0x0001D5D4 <= code && code <= 0x0001D5ED || 0x0001D608 <= code && code <= 0x0001D621 || 0x0001D63C <= code && code <= 0x0001D655 || 0x0001D670 <= code && code <= 0x0001D689 || 0x0001D6A8 <= code && code <= 0x0001D6C0 || 0x0001D6E2 <= code && code <= 0x0001D6FA || 0x0001D71C <= code && code <= 0x0001D734 || 0x0001D756 <= code && code <= 0x0001D76E || 0x0001D790 <= code && code <= 0x0001D7A8 || code == 0x0001D7CA
           )


isLatinAlphaNumOrUnderscoreFast : Char -> Bool
isLatinAlphaNumOrUnderscoreFast c =
    let
        code : Int
        code =
            Char.toCode c
    in
    charCodeIsLower code
        || charCodeIsUpper code
        || charCodeIsDigit code
        || (c == '_')


unicodeIsAlphaNumOrUnderscoreFast : Char -> Bool
unicodeIsAlphaNumOrUnderscoreFast c =
    let
        code : Int
        code =
            Char.toCode c
    in
    charCodeIsLower code
        || charCodeIsUpper code
        || charCodeIsDigit code
        || (c == '_')
        || (if code < 0x0100 then
                0x30 <= code && code <= 0x39 || 0x41 <= code && code <= 0x5A || 0x61 <= code && code <= 0x7A || code == 0xAA || 0xB2 <= code && code <= 0xB3 || code == 0xB5 || 0xB9 <= code && code <= 0xBA || 0xBC <= code && code <= 0xBE || 0xC0 <= code && code <= 0xD6 || 0xD8 <= code && code <= 0xF6 || 0xF8 <= code && code <= 0xFF

            else if code < 0xAAB4 then
                if code < 0x10FB then
                    if code < 0x0B34 then
                        if code < 0x093C then
                            if code < 0x0670 then
                                if code < 0x03A2 then
                                    0x0100 <= code && code <= 0x02C1 || 0x02C6 <= code && code <= 0x02D1 || 0x02E0 <= code && code <= 0x02E4 || 0x0370 <= code && code <= 0x0374 || 0x0376 <= code && code <= 0x0377 || 0x037A <= code && code <= 0x037D || code == 0x037F || code == 0x0386 || 0x0388 <= code && code <= 0x038A || code == 0x038C || 0x038E <= code && code <= 0x03A1 || modBy 2 code == 0 && 0x02EC <= code && code <= 0x02EE

                                else
                                    0x03A3 <= code && code <= 0x03F5 || 0x03F7 <= code && code <= 0x0481 || 0x048A <= code && code <= 0x052F || 0x0531 <= code && code <= 0x0556 || code == 0x0559 || 0x0560 <= code && code <= 0x0588 || 0x05D0 <= code && code <= 0x05EA || 0x05EF <= code && code <= 0x05F2 || 0x0620 <= code && code <= 0x064A || 0x0660 <= code && code <= 0x0669 || 0x066E <= code && code <= 0x066F

                            else if code < 0x07F9 then
                                0x0671 <= code && code <= 0x06D3 || code == 0x06D5 || 0x06E5 <= code && code <= 0x06E6 || 0x06EE <= code && code <= 0x06FC || code == 0x06FF || code == 0x0710 || 0x0712 <= code && code <= 0x072F || 0x074D <= code && code <= 0x07A5 || code == 0x07B1 || 0x07C0 <= code && code <= 0x07EA || 0x07F4 <= code && code <= 0x07F5

                            else
                                code == 0x07FA || 0x0800 <= code && code <= 0x0815 || code == 0x081A || code == 0x0824 || code == 0x0828 || 0x0840 <= code && code <= 0x0858 || 0x0860 <= code && code <= 0x086A || 0x0870 <= code && code <= 0x0887 || 0x0889 <= code && code <= 0x088E || 0x08A0 <= code && code <= 0x08C9 || 0x0904 <= code && code <= 0x0939

                        else if code < 0x0A31 then
                            if code < 0x09BC then
                                code == 0x093D || code == 0x0950 || 0x0958 <= code && code <= 0x0961 || 0x0966 <= code && code <= 0x096F || 0x0971 <= code && code <= 0x0980 || 0x0985 <= code && code <= 0x098C || 0x098F <= code && code <= 0x0990 || 0x0993 <= code && code <= 0x09A8 || 0x09AA <= code && code <= 0x09B0 || code == 0x09B2 || 0x09B6 <= code && code <= 0x09B9

                            else
                                code == 0x09BD || code == 0x09CE || 0x09DC <= code && code <= 0x09DD || 0x09DF <= code && code <= 0x09E1 || 0x09E6 <= code && code <= 0x09F1 || 0x09F4 <= code && code <= 0x09F9 || code == 0x09FC || 0x0A05 <= code && code <= 0x0A0A || 0x0A0F <= code && code <= 0x0A10 || 0x0A13 <= code && code <= 0x0A28 || 0x0A2A <= code && code <= 0x0A30

                        else if code < 0x0AB1 then
                            0x0A32 <= code && code <= 0x0A33 || 0x0A35 <= code && code <= 0x0A36 || 0x0A38 <= code && code <= 0x0A39 || 0x0A59 <= code && code <= 0x0A5C || code == 0x0A5E || 0x0A66 <= code && code <= 0x0A6F || 0x0A72 <= code && code <= 0x0A74 || 0x0A85 <= code && code <= 0x0A8D || 0x0A8F <= code && code <= 0x0A91 || 0x0A93 <= code && code <= 0x0AA8 || 0x0AAA <= code && code <= 0x0AB0

                        else
                            0x0AB2 <= code && code <= 0x0AB3 || 0x0AB5 <= code && code <= 0x0AB9 || code == 0x0ABD || code == 0x0AD0 || 0x0AE0 <= code && code <= 0x0AE1 || 0x0AE6 <= code && code <= 0x0AEF || code == 0x0AF9 || 0x0B05 <= code && code <= 0x0B0C || 0x0B0F <= code && code <= 0x0B10 || 0x0B13 <= code && code <= 0x0B28 || 0x0B2A <= code && code <= 0x0B30 || 0x0B32 <= code && code <= 0x0B33

                    else if code < 0x0D53 then
                        if code < 0x0C3C then
                            if code < 0x0B9B then
                                0x0B35 <= code && code <= 0x0B39 || code == 0x0B3D || 0x0B5C <= code && code <= 0x0B5D || 0x0B5F <= code && code <= 0x0B61 || 0x0B66 <= code && code <= 0x0B6F || 0x0B71 <= code && code <= 0x0B77 || code == 0x0B83 || 0x0B85 <= code && code <= 0x0B8A || 0x0B8E <= code && code <= 0x0B90 || 0x0B92 <= code && code <= 0x0B95 || 0x0B99 <= code && code <= 0x0B9A

                            else
                                code == 0x0B9C || 0x0B9E <= code && code <= 0x0B9F || 0x0BA3 <= code && code <= 0x0BA4 || 0x0BA8 <= code && code <= 0x0BAA || 0x0BAE <= code && code <= 0x0BB9 || code == 0x0BD0 || 0x0BE6 <= code && code <= 0x0BF2 || 0x0C05 <= code && code <= 0x0C0C || 0x0C0E <= code && code <= 0x0C10 || 0x0C12 <= code && code <= 0x0C28 || 0x0C2A <= code && code <= 0x0C39

                        else if code < 0x0CB4 then
                            code == 0x0C3D || 0x0C58 <= code && code <= 0x0C5A || code == 0x0C5D || 0x0C60 <= code && code <= 0x0C61 || 0x0C66 <= code && code <= 0x0C6F || 0x0C78 <= code && code <= 0x0C7E || code == 0x0C80 || 0x0C85 <= code && code <= 0x0C8C || 0x0C8E <= code && code <= 0x0C90 || 0x0C92 <= code && code <= 0x0CA8 || 0x0CAA <= code && code <= 0x0CB3

                        else
                            0x0CB5 <= code && code <= 0x0CB9 || code == 0x0CBD || 0x0CDD <= code && code <= 0x0CDE || 0x0CE0 <= code && code <= 0x0CE1 || 0x0CE6 <= code && code <= 0x0CEF || 0x0CF1 <= code && code <= 0x0CF2 || 0x0D04 <= code && code <= 0x0D0C || 0x0D0E <= code && code <= 0x0D10 || 0x0D12 <= code && code <= 0x0D3A || code == 0x0D3D || code == 0x0D4E

                    else if code < 0x0EBF then
                        if code < 0x0E31 then
                            0x0D54 <= code && code <= 0x0D56 || 0x0D58 <= code && code <= 0x0D61 || 0x0D66 <= code && code <= 0x0D78 || 0x0D7A <= code && code <= 0x0D7F || 0x0D85 <= code && code <= 0x0D96 || 0x0D9A <= code && code <= 0x0DB1 || 0x0DB3 <= code && code <= 0x0DBB || code == 0x0DBD || 0x0DC0 <= code && code <= 0x0DC6 || 0x0DE6 <= code && code <= 0x0DEF || 0x0E01 <= code && code <= 0x0E30

                        else
                            0x0E32 <= code && code <= 0x0E33 || 0x0E40 <= code && code <= 0x0E46 || 0x0E50 <= code && code <= 0x0E59 || 0x0E81 <= code && code <= 0x0E82 || code == 0x0E84 || 0x0E86 <= code && code <= 0x0E8A || 0x0E8C <= code && code <= 0x0EA3 || code == 0x0EA5 || 0x0EA7 <= code && code <= 0x0EB0 || 0x0EB2 <= code && code <= 0x0EB3 || code == 0x0EBD

                    else if code < 0x104F then
                        0x0EC0 <= code && code <= 0x0EC4 || code == 0x0EC6 || 0x0ED0 <= code && code <= 0x0ED9 || 0x0EDC <= code && code <= 0x0EDF || code == 0x0F00 || 0x0F20 <= code && code <= 0x0F33 || 0x0F40 <= code && code <= 0x0F47 || 0x0F49 <= code && code <= 0x0F6C || 0x0F88 <= code && code <= 0x0F8C || 0x1000 <= code && code <= 0x102A || 0x103F <= code && code <= 0x1049

                    else
                        0x1050 <= code && code <= 0x1055 || 0x105A <= code && code <= 0x105D || code == 0x1061 || 0x1065 <= code && code <= 0x1066 || 0x106E <= code && code <= 0x1070 || 0x1075 <= code && code <= 0x1081 || code == 0x108E || 0x1090 <= code && code <= 0x1099 || 0x10A0 <= code && code <= 0x10C5 || code == 0x10C7 || code == 0x10CD || 0x10D0 <= code && code <= 0x10FA

                else if code < 0x2106 then
                    if code < 0x197F then
                        if code < 0x1680 then
                            if code < 0x12C1 then
                                0x10FC <= code && code <= 0x1248 || 0x124A <= code && code <= 0x124D || 0x1250 <= code && code <= 0x1256 || code == 0x1258 || 0x125A <= code && code <= 0x125D || 0x1260 <= code && code <= 0x1288 || 0x128A <= code && code <= 0x128D || 0x1290 <= code && code <= 0x12B0 || 0x12B2 <= code && code <= 0x12B5 || 0x12B8 <= code && code <= 0x12BE || code == 0x12C0

                            else
                                0x12C2 <= code && code <= 0x12C5 || 0x12C8 <= code && code <= 0x12D6 || 0x12D8 <= code && code <= 0x1310 || 0x1312 <= code && code <= 0x1315 || 0x1318 <= code && code <= 0x135A || 0x1369 <= code && code <= 0x137C || 0x1380 <= code && code <= 0x138F || 0x13A0 <= code && code <= 0x13F5 || 0x13F8 <= code && code <= 0x13FD || 0x1401 <= code && code <= 0x166C || 0x166F <= code && code <= 0x167F

                        else if code < 0x17DF then
                            0x1681 <= code && code <= 0x169A || 0x16A0 <= code && code <= 0x16EA || 0x16EE <= code && code <= 0x16F8 || 0x1700 <= code && code <= 0x1711 || 0x171F <= code && code <= 0x1731 || 0x1740 <= code && code <= 0x1751 || 0x1760 <= code && code <= 0x176C || 0x176E <= code && code <= 0x1770 || 0x1780 <= code && code <= 0x17B3 || code == 0x17D7 || code == 0x17DC

                        else
                            0x17E0 <= code && code <= 0x17E9 || 0x17F0 <= code && code <= 0x17F9 || 0x1810 <= code && code <= 0x1819 || 0x1820 <= code && code <= 0x1878 || 0x1880 <= code && code <= 0x1884 || 0x1887 <= code && code <= 0x18A8 || code == 0x18AA || 0x18B0 <= code && code <= 0x18F5 || 0x1900 <= code && code <= 0x191E || 0x1946 <= code && code <= 0x196D || 0x1970 <= code && code <= 0x1974

                    else if code < 0x1CF9 then
                        if code < 0x1B82 then
                            0x1980 <= code && code <= 0x19AB || 0x19B0 <= code && code <= 0x19C9 || 0x19D0 <= code && code <= 0x19DA || 0x1A00 <= code && code <= 0x1A16 || 0x1A20 <= code && code <= 0x1A54 || 0x1A80 <= code && code <= 0x1A89 || 0x1A90 <= code && code <= 0x1A99 || code == 0x1AA7 || 0x1B05 <= code && code <= 0x1B33 || 0x1B45 <= code && code <= 0x1B4C || 0x1B50 <= code && code <= 0x1B59

                        else
                            0x1B83 <= code && code <= 0x1BA0 || 0x1BAE <= code && code <= 0x1BE5 || 0x1C00 <= code && code <= 0x1C23 || 0x1C40 <= code && code <= 0x1C49 || 0x1C4D <= code && code <= 0x1C7D || 0x1C80 <= code && code <= 0x1C88 || 0x1C90 <= code && code <= 0x1CBA || 0x1CBD <= code && code <= 0x1CBF || 0x1CE9 <= code && code <= 0x1CEC || 0x1CEE <= code && code <= 0x1CF3 || 0x1CF5 <= code && code <= 0x1CF6

                    else if code < 0x1FC1 then
                        code == 0x1CFA || 0x1D00 <= code && code <= 0x1DBF || 0x1E00 <= code && code <= 0x1F15 || 0x1F18 <= code && code <= 0x1F1D || 0x1F20 <= code && code <= 0x1F45 || 0x1F48 <= code && code <= 0x1F4D || 0x1F50 <= code && code <= 0x1F57 || 0x1F60 <= code && code <= 0x1F7D || 0x1F80 <= code && code <= 0x1FB4 || 0x1FB6 <= code && code <= 0x1FBC || code == 0x1FBE || modBy 2 code == 1 && 0x1F59 <= code && code <= 0x1F5F

                    else
                        0x1FC2 <= code && code <= 0x1FC4 || 0x1FC6 <= code && code <= 0x1FCC || 0x1FD0 <= code && code <= 0x1FD3 || 0x1FD6 <= code && code <= 0x1FDB || 0x1FE0 <= code && code <= 0x1FEC || 0x1FF2 <= code && code <= 0x1FF4 || 0x1FF6 <= code && code <= 0x1FFC || 0x2070 <= code && code <= 0x2071 || 0x2074 <= code && code <= 0x2079 || 0x207F <= code && code <= 0x2089 || 0x2090 <= code && code <= 0x209C || code == 0x2102

                else if code < 0x31EF then
                    if code < 0x2D7F then
                        if code < 0x24E9 then
                            code == 0x2107 || 0x210A <= code && code <= 0x2113 || code == 0x2115 || 0x2119 <= code && code <= 0x211D || 0x212A <= code && code <= 0x212D || 0x212F <= code && code <= 0x2139 || 0x213C <= code && code <= 0x213F || 0x2145 <= code && code <= 0x2149 || code == 0x214E || 0x2150 <= code && code <= 0x2189 || 0x2460 <= code && code <= 0x249B || modBy 2 code == 0 && 0x2124 <= code && code <= 0x2128

                        else
                            0x24EA <= code && code <= 0x24FF || 0x2776 <= code && code <= 0x2793 || 0x2C00 <= code && code <= 0x2CE4 || 0x2CEB <= code && code <= 0x2CEE || 0x2CF2 <= code && code <= 0x2CF3 || code == 0x2CFD || 0x2D00 <= code && code <= 0x2D25 || code == 0x2D27 || code == 0x2D2D || 0x2D30 <= code && code <= 0x2D67 || code == 0x2D6F

                    else if code < 0x3020 then
                        0x2D80 <= code && code <= 0x2D96 || 0x2DA0 <= code && code <= 0x2DA6 || 0x2DA8 <= code && code <= 0x2DAE || 0x2DB0 <= code && code <= 0x2DB6 || 0x2DB8 <= code && code <= 0x2DBE || 0x2DC0 <= code && code <= 0x2DC6 || 0x2DC8 <= code && code <= 0x2DCE || 0x2DD0 <= code && code <= 0x2DD6 || 0x2DD8 <= code && code <= 0x2DDE || code == 0x2E2F || 0x3005 <= code && code <= 0x3007

                    else
                        0x3021 <= code && code <= 0x3029 || 0x3031 <= code && code <= 0x3035 || 0x3038 <= code && code <= 0x303C || 0x3041 <= code && code <= 0x3096 || 0x309D <= code && code <= 0x309F || 0x30A1 <= code && code <= 0x30FA || 0x30FC <= code && code <= 0x30FF || 0x3105 <= code && code <= 0x312F || 0x3131 <= code && code <= 0x318E || 0x3192 <= code && code <= 0x3195 || 0x31A0 <= code && code <= 0x31BF

                else if code < 0xA80B then
                    if code < 0xA63F then
                        0x31F0 <= code && code <= 0x31FF || 0x3220 <= code && code <= 0x3229 || 0x3248 <= code && code <= 0x324F || 0x3251 <= code && code <= 0x325F || 0x3280 <= code && code <= 0x3289 || 0x32B1 <= code && code <= 0x32BF || 0x3400 <= code && code <= 0x4DBF || 0x4E00 <= code && code <= 0xA48C || 0xA4D0 <= code && code <= 0xA4FD || 0xA500 <= code && code <= 0xA60C || 0xA610 <= code && code <= 0xA62B

                    else
                        0xA640 <= code && code <= 0xA66E || 0xA67F <= code && code <= 0xA69D || 0xA6A0 <= code && code <= 0xA6EF || 0xA717 <= code && code <= 0xA71F || 0xA722 <= code && code <= 0xA788 || 0xA78B <= code && code <= 0xA7CA || 0xA7D0 <= code && code <= 0xA7D1 || 0xA7D6 <= code && code <= 0xA7D9 || 0xA7F2 <= code && code <= 0xA801 || 0xA803 <= code && code <= 0xA805 || 0xA807 <= code && code <= 0xA80A || modBy 2 code == 1 && 0xA7D3 <= code && code <= 0xA7D5

                else if code < 0xA983 then
                    0xA80C <= code && code <= 0xA822 || 0xA830 <= code && code <= 0xA835 || 0xA840 <= code && code <= 0xA873 || 0xA882 <= code && code <= 0xA8B3 || 0xA8D0 <= code && code <= 0xA8D9 || 0xA8F2 <= code && code <= 0xA8F7 || code == 0xA8FB || 0xA8FD <= code && code <= 0xA8FE || 0xA900 <= code && code <= 0xA925 || 0xA930 <= code && code <= 0xA946 || 0xA960 <= code && code <= 0xA97C

                else
                    0xA984 <= code && code <= 0xA9B2 || 0xA9CF <= code && code <= 0xA9D9 || 0xA9E0 <= code && code <= 0xA9E4 || 0xA9E6 <= code && code <= 0xA9FE || 0xAA00 <= code && code <= 0xAA28 || 0xAA40 <= code && code <= 0xAA42 || 0xAA44 <= code && code <= 0xAA4B || 0xAA50 <= code && code <= 0xAA59 || 0xAA60 <= code && code <= 0xAA76 || code == 0xAA7A || 0xAA7E <= code && code <= 0xAAAF || code == 0xAAB1

            else if code < 0x000116B7 then
                if code < 0x00010857 then
                    if code < 0x0001000C then
                        if code < 0xFB1E then
                            if code < 0xAB5B then
                                0xAAB5 <= code && code <= 0xAAB6 || 0xAAB9 <= code && code <= 0xAABD || 0xAADB <= code && code <= 0xAADD || 0xAAE0 <= code && code <= 0xAAEA || 0xAAF2 <= code && code <= 0xAAF4 || 0xAB01 <= code && code <= 0xAB06 || 0xAB09 <= code && code <= 0xAB0E || 0xAB11 <= code && code <= 0xAB16 || 0xAB20 <= code && code <= 0xAB26 || 0xAB28 <= code && code <= 0xAB2E || 0xAB30 <= code && code <= 0xAB5A || modBy 2 code == 0 && 0xAAC0 <= code && code <= 0xAAC2

                            else
                                0xAB5C <= code && code <= 0xAB69 || 0xAB70 <= code && code <= 0xABE2 || 0xABF0 <= code && code <= 0xABF9 || 0xAC00 <= code && code <= 0xD7A3 || 0xD7B0 <= code && code <= 0xD7C6 || 0xD7CB <= code && code <= 0xD7FB || 0xF900 <= code && code <= 0xFA6D || 0xFA70 <= code && code <= 0xFAD9 || 0xFB00 <= code && code <= 0xFB06 || 0xFB13 <= code && code <= 0xFB17 || code == 0xFB1D

                        else if code < 0xFE6F then
                            0xFB1F <= code && code <= 0xFB28 || 0xFB2A <= code && code <= 0xFB36 || 0xFB38 <= code && code <= 0xFB3C || code == 0xFB3E || 0xFB40 <= code && code <= 0xFB41 || 0xFB43 <= code && code <= 0xFB44 || 0xFB46 <= code && code <= 0xFBB1 || 0xFBD3 <= code && code <= 0xFD3D || 0xFD50 <= code && code <= 0xFD8F || 0xFD92 <= code && code <= 0xFDC7 || 0xFDF0 <= code && code <= 0xFDFB

                        else
                            0xFE70 <= code && code <= 0xFE74 || 0xFE76 <= code && code <= 0xFEFC || 0xFF10 <= code && code <= 0xFF19 || 0xFF21 <= code && code <= 0xFF3A || 0xFF41 <= code && code <= 0xFF5A || 0xFF66 <= code && code <= 0xFFBE || 0xFFC2 <= code && code <= 0xFFC7 || 0xFFCA <= code && code <= 0xFFCF || 0xFFD2 <= code && code <= 0xFFD7 || 0xFFDA <= code && code <= 0xFFDC || 0x00010000 <= code && code <= 0x0001000B

                    else if code < 0x000104D7 then
                        if code < 0x000102E0 then
                            0x0001000D <= code && code <= 0x00010026 || 0x00010028 <= code && code <= 0x0001003A || 0x0001003C <= code && code <= 0x0001003D || 0x0001003F <= code && code <= 0x0001004D || 0x00010050 <= code && code <= 0x0001005D || 0x00010080 <= code && code <= 0x000100FA || 0x00010107 <= code && code <= 0x00010133 || 0x00010140 <= code && code <= 0x00010178 || 0x0001018A <= code && code <= 0x0001018B || 0x00010280 <= code && code <= 0x0001029C || 0x000102A0 <= code && code <= 0x000102D0

                        else
                            0x000102E1 <= code && code <= 0x000102FB || 0x00010300 <= code && code <= 0x00010323 || 0x0001032D <= code && code <= 0x0001034A || 0x00010350 <= code && code <= 0x00010375 || 0x00010380 <= code && code <= 0x0001039D || 0x000103A0 <= code && code <= 0x000103C3 || 0x000103C8 <= code && code <= 0x000103CF || 0x000103D1 <= code && code <= 0x000103D5 || 0x00010400 <= code && code <= 0x0001049D || 0x000104A0 <= code && code <= 0x000104A9 || 0x000104B0 <= code && code <= 0x000104D3

                    else if code < 0x000105FF then
                        0x000104D8 <= code && code <= 0x000104FB || 0x00010500 <= code && code <= 0x00010527 || 0x00010530 <= code && code <= 0x00010563 || 0x00010570 <= code && code <= 0x0001057A || 0x0001057C <= code && code <= 0x0001058A || 0x0001058C <= code && code <= 0x00010592 || 0x00010594 <= code && code <= 0x00010595 || 0x00010597 <= code && code <= 0x000105A1 || 0x000105A3 <= code && code <= 0x000105B1 || 0x000105B3 <= code && code <= 0x000105B9 || 0x000105BB <= code && code <= 0x000105BC

                    else
                        0x00010600 <= code && code <= 0x00010736 || 0x00010740 <= code && code <= 0x00010755 || 0x00010760 <= code && code <= 0x00010767 || 0x00010780 <= code && code <= 0x00010785 || 0x00010787 <= code && code <= 0x000107B0 || 0x000107B2 <= code && code <= 0x000107BA || 0x00010800 <= code && code <= 0x00010805 || code == 0x00010808 || 0x0001080A <= code && code <= 0x00010835 || 0x00010837 <= code && code <= 0x00010838 || code == 0x0001083C || 0x0001083F <= code && code <= 0x00010855

                else if code < 0x000110EF then
                    if code < 0x00010B77 then
                        if code < 0x00010A14 then
                            0x00010858 <= code && code <= 0x00010876 || 0x00010879 <= code && code <= 0x0001089E || 0x000108A7 <= code && code <= 0x000108AF || 0x000108E0 <= code && code <= 0x000108F2 || 0x000108F4 <= code && code <= 0x000108F5 || 0x000108FB <= code && code <= 0x0001091B || 0x00010920 <= code && code <= 0x00010939 || 0x00010980 <= code && code <= 0x000109B7 || 0x000109BC <= code && code <= 0x000109CF || 0x000109D2 <= code && code <= 0x00010A00 || 0x00010A10 <= code && code <= 0x00010A13

                        else
                            0x00010A15 <= code && code <= 0x00010A17 || 0x00010A19 <= code && code <= 0x00010A35 || 0x00010A40 <= code && code <= 0x00010A48 || 0x00010A60 <= code && code <= 0x00010A7E || 0x00010A80 <= code && code <= 0x00010A9F || 0x00010AC0 <= code && code <= 0x00010AC7 || 0x00010AC9 <= code && code <= 0x00010AE4 || 0x00010AEB <= code && code <= 0x00010AEF || 0x00010B00 <= code && code <= 0x00010B35 || 0x00010B40 <= code && code <= 0x00010B55 || 0x00010B58 <= code && code <= 0x00010B72

                    else if code < 0x00010F2F then
                        0x00010B78 <= code && code <= 0x00010B91 || 0x00010BA9 <= code && code <= 0x00010BAF || 0x00010C00 <= code && code <= 0x00010C48 || 0x00010C80 <= code && code <= 0x00010CB2 || 0x00010CC0 <= code && code <= 0x00010CF2 || 0x00010CFA <= code && code <= 0x00010D23 || 0x00010D30 <= code && code <= 0x00010D39 || 0x00010E60 <= code && code <= 0x00010E7E || 0x00010E80 <= code && code <= 0x00010EA9 || 0x00010EB0 <= code && code <= 0x00010EB1 || 0x00010F00 <= code && code <= 0x00010F27

                    else
                        0x00010F30 <= code && code <= 0x00010F45 || 0x00010F51 <= code && code <= 0x00010F54 || 0x00010F70 <= code && code <= 0x00010F81 || 0x00010FB0 <= code && code <= 0x00010FCB || 0x00010FE0 <= code && code <= 0x00010FF6 || 0x00011003 <= code && code <= 0x00011037 || 0x00011052 <= code && code <= 0x0001106F || 0x00011071 <= code && code <= 0x00011072 || code == 0x00011075 || 0x00011083 <= code && code <= 0x000110AF || 0x000110D0 <= code && code <= 0x000110E8

                else if code < 0x00011304 then
                    if code < 0x000111E0 then
                        0x000110F0 <= code && code <= 0x000110F9 || 0x00011103 <= code && code <= 0x00011126 || 0x00011136 <= code && code <= 0x0001113F || code == 0x00011144 || code == 0x00011147 || 0x00011150 <= code && code <= 0x00011172 || code == 0x00011176 || 0x00011183 <= code && code <= 0x000111B2 || 0x000111C1 <= code && code <= 0x000111C4 || 0x000111D0 <= code && code <= 0x000111DA || code == 0x000111DC

                    else
                        0x000111E1 <= code && code <= 0x000111F4 || 0x00011200 <= code && code <= 0x00011211 || 0x00011213 <= code && code <= 0x0001122B || 0x0001123F <= code && code <= 0x00011240 || 0x00011280 <= code && code <= 0x00011286 || code == 0x00011288 || 0x0001128A <= code && code <= 0x0001128D || 0x0001128F <= code && code <= 0x0001129D || 0x0001129F <= code && code <= 0x000112A8 || 0x000112B0 <= code && code <= 0x000112DE || 0x000112F0 <= code && code <= 0x000112F9

                else if code < 0x0001144F then
                    0x00011305 <= code && code <= 0x0001130C || 0x0001130F <= code && code <= 0x00011310 || 0x00011313 <= code && code <= 0x00011328 || 0x0001132A <= code && code <= 0x00011330 || 0x00011332 <= code && code <= 0x00011333 || 0x00011335 <= code && code <= 0x00011339 || code == 0x0001133D || code == 0x00011350 || 0x0001135D <= code && code <= 0x00011361 || 0x00011400 <= code && code <= 0x00011434 || 0x00011447 <= code && code <= 0x0001144A

                else
                    0x00011450 <= code && code <= 0x00011459 || 0x0001145F <= code && code <= 0x00011461 || 0x00011480 <= code && code <= 0x000114AF || 0x000114C4 <= code && code <= 0x000114C5 || code == 0x000114C7 || 0x000114D0 <= code && code <= 0x000114D9 || 0x00011580 <= code && code <= 0x000115AE || 0x000115D8 <= code && code <= 0x000115DB || 0x00011600 <= code && code <= 0x0001162F || code == 0x00011644 || 0x00011650 <= code && code <= 0x00011659 || 0x00011680 <= code && code <= 0x000116AA

            else if code < 0x0001D455 then
                if code < 0x00011FFF then
                    if code < 0x00011BFF then
                        if code < 0x00011917 then
                            code == 0x000116B8 || 0x000116C0 <= code && code <= 0x000116C9 || 0x00011700 <= code && code <= 0x0001171A || 0x00011730 <= code && code <= 0x0001173B || 0x00011740 <= code && code <= 0x00011746 || 0x00011800 <= code && code <= 0x0001182B || 0x000118A0 <= code && code <= 0x000118F2 || 0x000118FF <= code && code <= 0x00011906 || code == 0x00011909 || 0x0001190C <= code && code <= 0x00011913 || 0x00011915 <= code && code <= 0x00011916

                        else
                            0x00011918 <= code && code <= 0x0001192F || 0x00011950 <= code && code <= 0x00011959 || 0x000119A0 <= code && code <= 0x000119A7 || 0x000119AA <= code && code <= 0x000119D0 || code == 0x00011A00 || 0x00011A0B <= code && code <= 0x00011A32 || code == 0x00011A3A || code == 0x00011A50 || 0x00011A5C <= code && code <= 0x00011A89 || code == 0x00011A9D || 0x00011AB0 <= code && code <= 0x00011AF8 || modBy 2 code == 1 && (0x0001193F <= code && code <= 0x00011941 || 0x000119E1 <= code && code <= 0x000119E3)

                    else if code < 0x00011D66 then
                        0x00011C00 <= code && code <= 0x00011C08 || 0x00011C0A <= code && code <= 0x00011C2E || code == 0x00011C40 || 0x00011C50 <= code && code <= 0x00011C6C || 0x00011C72 <= code && code <= 0x00011C8F || 0x00011D00 <= code && code <= 0x00011D06 || 0x00011D08 <= code && code <= 0x00011D09 || 0x00011D0B <= code && code <= 0x00011D30 || code == 0x00011D46 || 0x00011D50 <= code && code <= 0x00011D59 || 0x00011D60 <= code && code <= 0x00011D65

                    else
                        0x00011D67 <= code && code <= 0x00011D68 || 0x00011D6A <= code && code <= 0x00011D89 || code == 0x00011D98 || 0x00011DA0 <= code && code <= 0x00011DA9 || 0x00011EE0 <= code && code <= 0x00011EF2 || code == 0x00011F02 || 0x00011F04 <= code && code <= 0x00011F10 || 0x00011F12 <= code && code <= 0x00011F33 || 0x00011F50 <= code && code <= 0x00011F59 || code == 0x00011FB0 || 0x00011FC0 <= code && code <= 0x00011FD4

                else if code < 0x00016F92 then
                    if code < 0x00016ABF then
                        0x00012000 <= code && code <= 0x00012399 || 0x00012400 <= code && code <= 0x0001246E || 0x00012480 <= code && code <= 0x00012543 || 0x00012F90 <= code && code <= 0x00012FF0 || 0x00013000 <= code && code <= 0x0001342F || 0x00013441 <= code && code <= 0x00013446 || 0x00014400 <= code && code <= 0x00014646 || 0x00016800 <= code && code <= 0x00016A38 || 0x00016A40 <= code && code <= 0x00016A5E || 0x00016A60 <= code && code <= 0x00016A69 || 0x00016A70 <= code && code <= 0x00016ABE

                    else
                        0x00016AC0 <= code && code <= 0x00016AC9 || 0x00016AD0 <= code && code <= 0x00016AED || 0x00016B00 <= code && code <= 0x00016B2F || 0x00016B40 <= code && code <= 0x00016B43 || 0x00016B50 <= code && code <= 0x00016B59 || 0x00016B5B <= code && code <= 0x00016B61 || 0x00016B63 <= code && code <= 0x00016B77 || 0x00016B7D <= code && code <= 0x00016B8F || 0x00016E40 <= code && code <= 0x00016E96 || 0x00016F00 <= code && code <= 0x00016F4A || code == 0x00016F50

                else if code < 0x0001B14F then
                    0x00016F93 <= code && code <= 0x00016F9F || 0x00016FE0 <= code && code <= 0x00016FE1 || code == 0x00016FE3 || 0x00017000 <= code && code <= 0x000187F7 || 0x00018800 <= code && code <= 0x00018CD5 || 0x00018D00 <= code && code <= 0x00018D08 || 0x0001AFF0 <= code && code <= 0x0001AFF3 || 0x0001AFF5 <= code && code <= 0x0001AFFB || 0x0001AFFD <= code && code <= 0x0001AFFE || 0x0001B000 <= code && code <= 0x0001B122 || code == 0x0001B132

                else
                    0x0001B150 <= code && code <= 0x0001B152 || code == 0x0001B155 || 0x0001B164 <= code && code <= 0x0001B167 || 0x0001B170 <= code && code <= 0x0001B2FB || 0x0001BC00 <= code && code <= 0x0001BC6A || 0x0001BC70 <= code && code <= 0x0001BC7C || 0x0001BC80 <= code && code <= 0x0001BC88 || 0x0001BC90 <= code && code <= 0x0001BC99 || 0x0001D2C0 <= code && code <= 0x0001D2D3 || 0x0001D2E0 <= code && code <= 0x0001D2F3 || 0x0001D360 <= code && code <= 0x0001D378 || 0x0001D400 <= code && code <= 0x0001D454

            else if code < 0x0001E7EF then
                if code < 0x0001D715 then
                    if code < 0x0001D515 then
                        0x0001D456 <= code && code <= 0x0001D49C || 0x0001D49E <= code && code <= 0x0001D49F || code == 0x0001D4A2 || 0x0001D4A5 <= code && code <= 0x0001D4A6 || 0x0001D4A9 <= code && code <= 0x0001D4AC || 0x0001D4AE <= code && code <= 0x0001D4B9 || code == 0x0001D4BB || 0x0001D4BD <= code && code <= 0x0001D4C3 || 0x0001D4C5 <= code && code <= 0x0001D505 || 0x0001D507 <= code && code <= 0x0001D50A || 0x0001D50D <= code && code <= 0x0001D514

                    else
                        0x0001D516 <= code && code <= 0x0001D51C || 0x0001D51E <= code && code <= 0x0001D539 || 0x0001D53B <= code && code <= 0x0001D53E || 0x0001D540 <= code && code <= 0x0001D544 || code == 0x0001D546 || 0x0001D54A <= code && code <= 0x0001D550 || 0x0001D552 <= code && code <= 0x0001D6A5 || 0x0001D6A8 <= code && code <= 0x0001D6C0 || 0x0001D6C2 <= code && code <= 0x0001D6DA || 0x0001D6DC <= code && code <= 0x0001D6FA || 0x0001D6FC <= code && code <= 0x0001D714

                else if code < 0x0001E0FF then
                    0x0001D716 <= code && code <= 0x0001D734 || 0x0001D736 <= code && code <= 0x0001D74E || 0x0001D750 <= code && code <= 0x0001D76E || 0x0001D770 <= code && code <= 0x0001D788 || 0x0001D78A <= code && code <= 0x0001D7A8 || 0x0001D7AA <= code && code <= 0x0001D7C2 || 0x0001D7C4 <= code && code <= 0x0001D7CB || 0x0001D7CE <= code && code <= 0x0001D7FF || 0x0001DF00 <= code && code <= 0x0001DF1E || 0x0001DF25 <= code && code <= 0x0001DF2A || 0x0001E030 <= code && code <= 0x0001E06D

                else
                    0x0001E100 <= code && code <= 0x0001E12C || 0x0001E137 <= code && code <= 0x0001E13D || 0x0001E140 <= code && code <= 0x0001E149 || code == 0x0001E14E || 0x0001E290 <= code && code <= 0x0001E2AD || 0x0001E2C0 <= code && code <= 0x0001E2EB || 0x0001E2F0 <= code && code <= 0x0001E2F9 || 0x0001E4D0 <= code && code <= 0x0001E4EB || 0x0001E4F0 <= code && code <= 0x0001E4F9 || 0x0001E7E0 <= code && code <= 0x0001E7E6 || 0x0001E7E8 <= code && code <= 0x0001E7EB || 0x0001E7ED <= code && code <= 0x0001E7EE

            else if code < 0x0001EE60 then
                if code < 0x0001EDFF then
                    0x0001E7F0 <= code && code <= 0x0001E7FE || 0x0001E800 <= code && code <= 0x0001E8C4 || 0x0001E8C7 <= code && code <= 0x0001E8CF || 0x0001E900 <= code && code <= 0x0001E943 || code == 0x0001E94B || 0x0001E950 <= code && code <= 0x0001E959 || 0x0001EC71 <= code && code <= 0x0001ECAB || 0x0001ECAD <= code && code <= 0x0001ECAF || 0x0001ECB1 <= code && code <= 0x0001ECB4 || 0x0001ED01 <= code && code <= 0x0001ED2D || 0x0001ED2F <= code && code <= 0x0001ED3D

                else
                    0x0001EE00 <= code && code <= 0x0001EE03 || 0x0001EE05 <= code && code <= 0x0001EE1F || 0x0001EE21 <= code && code <= 0x0001EE22 || code == 0x0001EE24 || code == 0x0001EE27 || 0x0001EE29 <= code && code <= 0x0001EE32 || 0x0001EE34 <= code && code <= 0x0001EE37 || code == 0x0001EE42 || 0x0001EE4D <= code && code <= 0x0001EE4F || 0x0001EE51 <= code && code <= 0x0001EE52 || code == 0x0001EE54 || modBy 2 code == 1 && (0x0001EE39 <= code && code <= 0x0001EE3B || 0x0001EE47 <= code && code <= 0x0001EE4B || 0x0001EE57 <= code && code <= 0x0001EE5F)

            else if code < 0x0001EEAA then
                0x0001EE61 <= code && code <= 0x0001EE62 || code == 0x0001EE64 || 0x0001EE67 <= code && code <= 0x0001EE6A || 0x0001EE6C <= code && code <= 0x0001EE72 || 0x0001EE74 <= code && code <= 0x0001EE77 || 0x0001EE79 <= code && code <= 0x0001EE7C || code == 0x0001EE7E || 0x0001EE80 <= code && code <= 0x0001EE89 || 0x0001EE8B <= code && code <= 0x0001EE9B || 0x0001EEA1 <= code && code <= 0x0001EEA3 || 0x0001EEA5 <= code && code <= 0x0001EEA9

            else
                0x0001EEAB <= code && code <= 0x0001EEBB || 0x0001F100 <= code && code <= 0x0001F10C || 0x0001FBF0 <= code && code <= 0x0001FBF9 || 0x00020000 <= code && code <= 0x0002A6DF || 0x0002A700 <= code && code <= 0x0002B739 || 0x0002B740 <= code && code <= 0x0002B81D || 0x0002B820 <= code && code <= 0x0002CEA1 || 0x0002CEB0 <= code && code <= 0x0002EBE0 || 0x0002EBF0 <= code && code <= 0x0002EE5D || 0x0002F800 <= code && code <= 0x0002FA1D || 0x00030000 <= code && code <= 0x0003134A || 0x00031350 <= code && code <= 0x000323AF
           )


charCodeIsLower : Int -> Bool
charCodeIsLower code =
    0x61 <= code && code <= 0x7A


charCodeIsUpper : Int -> Bool
charCodeIsUpper code =
    code <= 0x5A && 0x41 <= code


charCodeIsDigit : Int -> Bool
charCodeIsDigit code =
    code <= 0x39 && 0x30 <= code
