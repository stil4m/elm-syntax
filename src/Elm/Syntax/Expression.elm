module Elm.Syntax.Expression
    exposing
        ( Expression
        , InnerExpression
            ( UnitExpr
            , Application
            , OperatorApplication
            , FunctionOrValue
            , IfBlock
            , PrefixOperator
            , Operator
            , Integer
            , Floatable
            , Negation
            , Literal
            , CharLiteral
            , TupledExpression
            , ParenthesizedExpression
            , LetExpression
            , CaseExpression
            , LambdaExpression
            , RecordExpr
            , ListExpr
            , QualifiedExpr
            , RecordAccess
            , RecordAccessFunction
            , RecordUpdateExpression
            , GLSLExpression
            )
        , Function
        , FunctionSignature
        , FunctionDeclaration
        , CaseBlock
        , Cases
        , Case
        , Lambda
        , LetBlock
        , LetDeclaration
            ( LetFunction
            , LetDestructuring
            )
        , RecordUpdate
        , RecordSetter
        )

{-| Expression Syntax


# Expression

@docs Expression, InnerExpression, Lambda


# Lets

@docs LetBlock, LetDeclaration


# Records

@docs RecordUpdate,RecordSetter


# Cases

@docs CaseBlock, Cases, Case


# Functions

@docs Function, FunctionDeclaration, FunctionSignature

-}

import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.Base exposing (VariablePointer, ModuleName)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Infix exposing (InfixDirection)


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
    , arguments : List Pattern
    , expression : Expression
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
type alias Expression =
    ( Range, InnerExpression )


{-| Union type containing all possible expressions
-}
type InnerExpression
    = UnitExpr
    | Application (List Expression)
    | OperatorApplication String InfixDirection Expression Expression
    | FunctionOrValue String
    | IfBlock Expression Expression Expression
    | PrefixOperator String
    | Operator String
    | Integer Int
    | Floatable Float
    | Negation Expression
    | Literal String
    | CharLiteral Char
    | TupledExpression (List Expression)
    | ParenthesizedExpression Expression
    | LetExpression LetBlock
    | CaseExpression CaseBlock
    | LambdaExpression Lambda
    | RecordExpr (List RecordSetter)
    | ListExpr (List Expression)
    | QualifiedExpr ModuleName String
    | RecordAccess Expression String
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
    ( String, Expression )


{-| Expression for a let block
-}
type alias LetBlock =
    { declarations : List LetDeclaration
    , expression : Expression
    }


{-| Union type for all possible declations in a let block
-}
type LetDeclaration
    = LetFunction Function
    | LetDestructuring Pattern Expression


{-| Expression for a lambda
-}
type alias Lambda =
    { args : List Pattern
    , expression : Expression
    }


{-| Expression for a case block
-}
type alias CaseBlock =
    { expression : Expression
    , cases : Cases
    }


{-| A case in a case block
-}
type alias Case =
    ( Pattern, Expression )


{-| Type alias for a list of cases
-}
type alias Cases =
    List Case
