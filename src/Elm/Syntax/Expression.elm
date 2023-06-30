module Elm.Syntax.Expression exposing
    ( Expression(..), Lambda, LetBlock, LetDeclaration(..), RecordSetter, CaseBlock, Case, Function, FunctionImplementation
    , functionRange, isLambda, isLet, isIfElse, isCase, isOperatorApplication
    )

{-| This syntax represents all that you can express in Elm.
Although it is a easy and simple language, you can express a lot! See the `Expression` type for all the things you can express.


## Types

@docs Expression, Lambda, LetBlock, LetDeclaration, RecordSetter, CaseBlock, Case, Function, FunctionImplementation


## Functions

@docs functionRange, isLambda, isLet, isIfElse, isCase, isOperatorApplication

-}

import Elm.Syntax.DestructurePattern exposing (DestructurePattern)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.StringLiteralType exposing (StringLiteralType)


{-| Type alias for a full function
-}
type alias Function =
    { documentation : Maybe (Node Documentation)
    , signature : Maybe (Node Signature)
    , declaration : Node FunctionImplementation
    }


{-| Get the full range of a function
-}
functionRange : Function -> Range
functionRange function =
    let
        declarationRange : Range
        declarationRange =
            Node.range function.declaration

        startRange : Range
        startRange =
            case function.documentation of
                Just (Node range _) ->
                    range

                Nothing ->
                    case function.signature of
                        Just (Node range _) ->
                            range

                        Nothing ->
                            declarationRange
    in
    { start = startRange.start
    , end = declarationRange.end
    }


{-| Type alias for a function's implementation
-}
type alias FunctionImplementation =
    { name : Node String
    , arguments : List (Node DestructurePattern)
    , expression : Node Expression
    }


{-| Custom type for all expressions such as:

  - `Application`: `add a b`
  - `Operation`: `a + b`
  - `FunctionOrValue`: `add` or `True`
  - `If`: `if a then b else c`
  - `PrefixOperator`: `(+)`
  - `Operator`: `+` (not possible to get in practice)
  - `IntegerLiteral`: `42`
  - `HexLiteral`: `0x1F`
  - `FloatLiteral`: `42.0`
  - `Negation`: `-a`
  - `StringLiteral`: `"text"` or `"""text"""`
  - `CharLiteral`: `'a'`
  - `TupleExpression`: Something wrapped in parentheses like unit `()`, parentheses `(a)`, or a tuple `( a, b )`
  - `LetExpression`: `let a = 4 in a`
  - `Case`: `case a of` followed by pattern matches
  - `LambdaExpression`: `(\a -> a)`
  - `Record`: `{ name = "text" }`
  - `ListLiteral`: `[ x, y ]`
  - `RecordAccess`: `a.name`
  - `RecordAccessFunction`: `.name`
  - `RecordUpdate`: `{ a | name = "text" }`
  - `GLSLExpression`: `[glsl| ... |]`

-}
type Expression
    = Application (Node Expression) (List (Node Expression))
    | Operation String InfixDirection (Node Expression) (Node Expression)
    | FunctionOrValue ModuleName String
    | If (Node Expression) (Node Expression) (Node Expression)
    | PrefixOperator String
    | Operator String
    | IntegerLiteral Int
    | HexLiteral Int
    | FloatLiteral Float
    | Negation (Node Expression)
    | StringLiteral StringLiteralType String
    | CharLiteral Char
    | TupleExpression (List (Node Expression))
    | LetExpression LetBlock
    | Case CaseBlock
    | LambdaExpression Lambda
    | Record (List (Node RecordSetter))
    | ListLiteral (List (Node Expression))
    | RecordAccess (Node Expression) (Node String)
    | RecordAccessFunction String
    | RecordUpdate (Node String) (Node RecordSetter) (List (Node RecordSetter))
    | GLSLExpression String


{-| Expression for setting a record field
-}
type alias RecordSetter =
    ( Node String, Node Expression )


{-| Expression for a let block
-}
type alias LetBlock =
    { declarations : List (Node LetDeclaration)
    , expression : Node Expression
    }


{-| Union type for all possible declarations in a let block
-}
type LetDeclaration
    = LetFunction Function
    | LetDestructuring (Node DestructurePattern) (Node Expression)


{-| Expression for a lambda
-}
type alias Lambda =
    { firstArg : Node DestructurePattern
    , restOfArgs : List (Node DestructurePattern)
    , expression : Node Expression
    }


{-| Expression for a case block
-}
type alias CaseBlock =
    { expression : Node Expression
    , firstCase : Case
    , restOfCases : List Case
    }


{-| A case in a case block
-}
type alias Case =
    ( Node Pattern, Node Expression )


{-| Check whether an expression is a lambda-expression
-}
isLambda : Expression -> Bool
isLambda e =
    case e of
        LambdaExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is a let-expression
-}
isLet : Expression -> Bool
isLet e =
    case e of
        LetExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is an if-else-expression
-}
isIfElse : Expression -> Bool
isIfElse e =
    case e of
        If _ _ _ ->
            True

        _ ->
            False


{-| Check whether an expression is a case-expression
-}
isCase : Expression -> Bool
isCase e =
    case e of
        Case _ ->
            True

        _ ->
            False


{-| Check whether an expression is an operator application expression
-}
isOperatorApplication : Expression -> Bool
isOperatorApplication e =
    case e of
        Operation _ _ _ _ ->
            True

        _ ->
            False
