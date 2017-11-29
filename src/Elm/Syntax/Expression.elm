module Elm.Syntax.Expression
    exposing
        ( Case
        , CaseBlock
        , Cases
        , Expression
            ( Application
            , CaseExpression
            , CharLiteral
            , Floatable
            , FunctionOrValue
            , GLSLExpression
            , IfBlock
            , Integer
            , LambdaExpression
            , LetExpression
            , ListExpr
            , Literal
            , Negation
            , Operator
            , OperatorApplication
            , ParenthesizedExpression
            , PrefixOperator
            , QualifiedExpr
            , RecordAccess
            , RecordAccessFunction
            , RecordExpr
            , RecordUpdateExpression
            , TupledExpression
            , UnitExpr
            )
        , Function
        , FunctionDeclaration
        , FunctionSignature
        , Lambda
        , LetBlock
        , LetDeclaration
            ( LetDestructuring
            , LetFunction
            )
        , RecordSetter
        , RecordUpdate
        )

{-| Expression Syntax


# Expression

@docs Expression, Lambda


# Lets

@docs LetBlock, LetDeclaration


# Records

@docs RecordUpdate,RecordSetter


# Cases

@docs CaseBlock, Cases, Case


# Functions

@docs Function, FunctionDeclaration, FunctionSignature

-}

import Elm.Syntax.Base exposing (ModuleName, VariablePointer)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)


{-| Type alias for a full function
-}
type alias Function =
    { documentation : Maybe Documentation
    , signature : Maybe FunctionSignature
    , declaration : FunctionDeclaration
    }


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
    , name : String
    , typeAnnotation : TypeAnnotation
    , range : Range
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
    { declarations : List LetDeclaration
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
