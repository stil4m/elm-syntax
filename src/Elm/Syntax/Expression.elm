module Elm.Syntax.Expression
    exposing
        ( Case
        , CaseBlock
        , Cases
        , Expression
        , Function
        , FunctionDeclaration
        , FunctionSignature
        , InnerExpression
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

import Elm.Syntax.Base exposing (ModuleName, VariablePointer)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
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
