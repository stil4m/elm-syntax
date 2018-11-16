# elm-syntax

Elm Syntax in Elm: for parsing and writing Elm in Elm.

Used in:

* [elm-analyse](https://github.com/stil4m/elm-analyse)
* [elm-lens](https://github.com/mbuscemi/elm-lens)


## Example

```elm
import Elm.Parser


src = """module Foo exposing(foo)

foo = 1
"""

parse : String -> String
parse input =
  case Elm.Parser.parse input of
    Err e ->
      "Failed: " ++ Debug.toString e
    Ok v ->
      "Success: " ++ Debug.toString v

main =
  parse src
```
