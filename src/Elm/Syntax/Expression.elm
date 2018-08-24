module Elm.Syntax.Expression exposing
    ( Expression(..), Lambda
    , LetBlock, LetDeclaration(..)
    , RecordUpdate, RecordSetter
    , CaseBlock, Cases, Case
    , Function, FunctionDeclaration, FunctionSignature, functionRange
    , isLambda, isLet, isIfElse, isCase, isOperatorApplication
    )

{-| Expression Syntax


# Expression

@docs Expression, Lambda


# Lets

@docs LetBlock, LetDeclaration


# Records

@docs RecordUpdate, RecordSetter


# Cases

@docs CaseBlock, Cases, Case


# Functions

@docs Function, FunctionDeclaration, FunctionSignature, functionRange


# Utiltity functions

@docs isLambda, isLet, isIfElse, isCase, isOperatorApplication

-}

import Elm.Syntax.Base exposing (ModuleName, VariablePointer)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)


{-| Type alias for a full function
-}
type alias Function =
    { documentation : Maybe Documentation
    , signature : Maybe (Ranged FunctionSignature)
    , declaration : FunctionDeclaration
    }


{-| Get the full range of a function
-}
functionRange : Function -> Range
functionRange function =
    Range.combine
        [ case function.documentation of
            Just d ->
                d.range

            Nothing ->
                function.signature
                    |> Maybe.map (Tuple.second >> .name >> .range)
                    |> Maybe.withDefault function.declaration.name.range
        , Tuple.first function.declaration.expression
        ]


{-| Type alias for declaring a function
-}
type alias FunctionDeclaration =
    { operatorDefinition : Bool
    , name : VariablePointer
    , arguments : List (Ranged Pattern)
    , expression : Ranged Expression
    }


{-| Type alias for a function signature
-}
type alias FunctionSignature =
    { operatorDefinition : Bool
    , name : VariablePointer
    , typeAnnotation : Ranged TypeAnnotation
    }


{-| Wrapper type for an expresion on a certain range
-}
type Expression
    = UnitExpr
    | Application (List (Ranged Expression))
    | OperatorApplication String InfixDirection (Ranged Expression) (Ranged Expression)
    | FunctionOrValue String
    | IfBlock (Ranged Expression) (Ranged Expression) (Ranged Expression)
    | PrefixOperator String
    | Operator String
    | Integer Int
    | Hex Int
    | Floatable Float
    | Negation (Ranged Expression)
    | Literal String
    | CharLiteral Char
    | TupledExpression (List (Ranged Expression))
    | ParenthesizedExpression (Ranged Expression)
    | LetExpression LetBlock
    | CaseExpression CaseBlock
    | LambdaExpression Lambda
    | RecordExpr (List RecordSetter)
    | ListExpr (List (Ranged Expression))
    | QualifiedExpr ModuleName String
    | RecordAccess (Ranged Expression) String
    | RecordAccessFunction String
    | RecordUpdateExpression RecordUpdate
    | GLSLExpression String


{-| Expression for updating a record
-}
type alias RecordUpdate =
    { name : String
    , updates : List RecordSetter
    }


{-| Expression for setting a record field
-}
type alias RecordSetter =
    ( String, Ranged Expression )


{-| Expression for a let block
-}
type alias LetBlock =
    { declarations : List (Ranged LetDeclaration)
    , expression : Ranged Expression
    }


{-| Union type for all possible declations in a let block
-}
type LetDeclaration
    = LetFunction Function
    | LetDestructuring (Ranged Pattern) (Ranged Expression)


{-| Expression for a lambda
-}
type alias Lambda =
    { args : List (Ranged Pattern)
    , expression : Ranged Expression
    }


{-| Expression for a case block
-}
type alias CaseBlock =
    { expression : Ranged Expression
    , cases : Cases
    }


{-| A case in a case block
-}
type alias Case =
    ( Ranged Pattern, Ranged Expression )


{-| Type alias for a list of cases
-}
type alias Cases =
    List Case


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
        IfBlock _ _ _ ->
            True

        _ ->
            False


{-| Check whether an expression is a case-expression
-}
isCase : Expression -> Bool
isCase e =
    case e of
        CaseExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is an operator appliation expression
-}
isOperatorApplication : Expression -> Bool
isOperatorApplication e =
    case e of
        OperatorApplication _ _ _ _ ->
            True

        _ ->
            False
