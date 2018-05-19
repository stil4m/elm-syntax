# elm-syntax

Elm Syntax in Elm: for parsing and writing Elm in Elm.

Used in:

* [elm-analyse](https://github.com/stil4m/elm-analyse)
* [elm-lens](https://github.com/mbuscemi/elm-lens)

# Usage

```
import Elm.Parser



let 
   parseResult = Elm.Parser.parse "module Foo exposing (..)\n ..."
in
case parseResult of
  Ok v ->
    -- Parse succeeded !
    Debug.toString v
  Err e ->
    -- Do some error handling
    Debug.toString e
```

