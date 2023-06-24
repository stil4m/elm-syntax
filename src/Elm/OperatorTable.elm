module Elm.OperatorTable exposing
    ( OperatorTable
    , table
    )

import Dict exposing (Dict)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import List


type alias OperatorTable =
    Dict String Infix


table : OperatorTable
table =
    List.foldl
        (\infix_ acc ->
            Dict.insert (Node.value infix_.operator) infix_ acc
        )
        Dict.empty
        [ -- elm/core
          -- Basics module
          -- infix right 0 (<|) = apL
          { direction = Node Range.emptyRange Right
          , precedence = Node Range.emptyRange 0
          , operator = Node Range.emptyRange "<|"
          , function = Node Range.emptyRange "apL"
          }
        , -- infix left  0 (|>) = apR
          { direction = Node Range.emptyRange Left
          , precedence = Node Range.emptyRange 0
          , operator = Node Range.emptyRange "|>"
          , function = Node Range.emptyRange "apR"
          }
        , -- infix right 2 (||) = or
          { direction = Node Range.emptyRange Right
          , precedence = Node Range.emptyRange 2
          , operator = Node Range.emptyRange "||"
          , function = Node Range.emptyRange "or"
          }
        , -- infix right 3 (&&) = and
          { direction = Node Range.emptyRange Right
          , precedence = Node Range.emptyRange 3
          , operator = Node Range.emptyRange "&&"
          , function = Node Range.emptyRange "and"
          }
        , -- infix non   4 (==) = eq
          { direction = Node Range.emptyRange Non
          , precedence = Node Range.emptyRange 4
          , operator = Node Range.emptyRange "=="
          , function = Node Range.emptyRange "eq"
          }
        , -- infix non   4 (/=) = neq
          { direction = Node Range.emptyRange Non
          , precedence = Node Range.emptyRange 4
          , operator = Node Range.emptyRange "/="
          , function = Node Range.emptyRange "neq"
          }
        , -- infix non   4 (<)  = lt
          { direction = Node Range.emptyRange Non
          , precedence = Node Range.emptyRange 4
          , operator = Node Range.emptyRange "<"
          , function = Node Range.emptyRange "lt"
          }
        , -- infix non   4 (>)  = gt
          { direction = Node Range.emptyRange Non
          , precedence = Node Range.emptyRange 4
          , operator = Node Range.emptyRange ">"
          , function = Node Range.emptyRange "gt"
          }
        , -- infix non   4 (<=) = le
          { direction = Node Range.emptyRange Non
          , precedence = Node Range.emptyRange 4
          , operator = Node Range.emptyRange "<="
          , function = Node Range.emptyRange "le"
          }
        , -- infix non   4 (>=) = ge
          { direction = Node Range.emptyRange Non
          , precedence = Node Range.emptyRange 4
          , operator = Node Range.emptyRange ">="
          , function = Node Range.emptyRange "ge"
          }
        , -- infix right 5 (++) = append
          { direction = Node Range.emptyRange Right
          , precedence = Node Range.emptyRange 5
          , operator = Node Range.emptyRange "++"
          , function = Node Range.emptyRange "append"
          }
        , -- infix left  6 (+)  = add
          { direction = Node Range.emptyRange Left
          , precedence = Node Range.emptyRange 6
          , operator = Node Range.emptyRange "+"
          , function = Node Range.emptyRange "add"
          }
        , -- infix left  6 (-)  = sub
          { direction = Node Range.emptyRange Left
          , precedence = Node Range.emptyRange 6
          , operator = Node Range.emptyRange "-"
          , function = Node Range.emptyRange "sub"
          }
        , -- infix left  7 (*)  = mul
          { direction = Node Range.emptyRange Left
          , precedence = Node Range.emptyRange 7
          , operator = Node Range.emptyRange "*"
          , function = Node Range.emptyRange "mul"
          }
        , -- infix left  7 (/)  = fdiv
          { direction = Node Range.emptyRange Left
          , precedence = Node Range.emptyRange 7
          , operator = Node Range.emptyRange "/"
          , function = Node Range.emptyRange "fdiv"
          }
        , -- infix left  7 (//) = idiv
          { direction = Node Range.emptyRange Left
          , precedence = Node Range.emptyRange 7
          , operator = Node Range.emptyRange "//"
          , function = Node Range.emptyRange "idiv"
          }
        , -- infix right 8 (^)  = pow
          { direction = Node Range.emptyRange Right
          , precedence = Node Range.emptyRange 8
          , operator = Node Range.emptyRange "^"
          , function = Node Range.emptyRange "pow"
          }
        , -- infix left  9 (<<) = composeL
          { direction = Node Range.emptyRange Left
          , precedence = Node Range.emptyRange 9
          , operator = Node Range.emptyRange "<<"
          , function = Node Range.emptyRange "composeL"
          }
        , -- infix right 9 (>>) = composeR
          { direction = Node Range.emptyRange Right
          , precedence = Node Range.emptyRange 9
          , operator = Node Range.emptyRange ">>"
          , function = Node Range.emptyRange "composeR"
          }
        , -- List module
          -- infix right 5 (::) = cons
          { direction = Node Range.emptyRange Right
          , precedence = Node Range.emptyRange 5
          , operator = Node Range.emptyRange "::"
          , function = Node Range.emptyRange "cons"
          }
        , -- elm/url
          -- Url.Parser module
          -- infix right 7 (</>) = slash
          { direction = Node Range.emptyRange Right
          , precedence = Node Range.emptyRange 7
          , operator = Node Range.emptyRange "</>"
          , function = Node Range.emptyRange "slash"
          }
        , -- infix left  8 (<?>) = questionMark
          { direction = Node Range.emptyRange Left
          , precedence = Node Range.emptyRange 8
          , operator = Node Range.emptyRange "<?>"
          , function = Node Range.emptyRange "questionMark"
          }
        , -- elm/parser
          -- Parser and Parser.Advanced modules
          -- infix left 5 (|=) = keeper
          { direction = Node Range.emptyRange Left
          , precedence = Node Range.emptyRange 5
          , operator = Node Range.emptyRange "|="
          , function = Node Range.emptyRange "keeper"
          }
        , -- infix left 6 (|.) = ignorer
          { direction = Node Range.emptyRange Left
          , precedence = Node Range.emptyRange 6
          , operator = Node Range.emptyRange "|."
          , function = Node Range.emptyRange "ignorer"
          }
        ]
