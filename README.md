# elm-syntax

Elm Syntax in Elm: for parsing and writing Elm in Elm.

## How does this work?

When Elm code is parsed, it's converted into an Abstract Syntax Tree (AST).
The AST lets us represent the code in a way that's much easier to work with when programming.

Here's an example of that:
Code: `3 + 4 * 2`
AST:
```elm
OperatorApplication
    (Integer 3)
    "+"
    (OperatorApplication
        (Integer 4)
        "*"
        (Integer 2)
    )
```

Notice how it forms a tree structure where we first multiply together 4 and 2, and then add the result with 3.
That's where the "tree" part of AST comes from.

## Getting Started

```elm
import Elm.Parser
import Html

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
  Html.text (parse src)
```

Used in:

* [elm-analyse](https://github.com/stil4m/elm-analyse)
* [elm-review](https://github.com/jfmengels/elm-review)
* [elm-xref](https://github.com/zwilias/elm-xref)
* [elm-lens](https://github.com/mbuscemi/elm-lens)
