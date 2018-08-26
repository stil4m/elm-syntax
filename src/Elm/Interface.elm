module Elm.Interface exposing
    ( Interface, Exposed(..)
    , build, exposesAlias, exposesFunction, operators
    )

{-|


# Elm.Interface


## Types

@docs Interface, Exposed


## Functions

@docs build, exposesAlias, exposesFunction, operators

-}

import Elm.Internal.RawFile exposing (RawFile(..))
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.Module as Module
import Elm.Syntax.Ranged exposing (Ranged)
import List.Extra


{-| An interface
-}
type alias Interface =
    List Exposed


{-| Union type for the things that a module can expose
-}
type Exposed
    = Function String
    | Type ( String, List String )
    | Alias String
    | Operator Infix


{-| Interface property whether a certain alias is exposed.
-}
exposesAlias : String -> Interface -> Bool
exposesAlias k interface =
    interface
        |> List.any
            (\x ->
                case x of
                    Alias l ->
                        k == l

                    _ ->
                        False
            )


{-| Interface property whether a certain function is exposed.
-}
exposesFunction : String -> Interface -> Bool
exposesFunction k interface =
    interface
        |> List.any
            (\x ->
                case x of
                    Function l ->
                        k == l

                    Type ( _, constructors ) ->
                        List.member k constructors

                    Operator inf ->
                        Tuple.second inf.operator == k

                    Alias _ ->
                        False
            )


{-| Retrieve all infix operators exposed
-}
operators : Interface -> List Infix
operators =
    List.filterMap
        (\i ->
            case i of
                Operator o ->
                    Just o

                _ ->
                    Nothing
        )


{-| Build an interface from a file
-}
build : RawFile -> Interface
build (Raw file) =
    let
        fileDefinitionList =
            fileToDefinitions file

        moduleExposing =
            Module.exposingList file.moduleDefinition
    in
    case moduleExposing of
        Explicit x ->
            buildInterfaceFromExplicit x fileDefinitionList

        All _ ->
            fileDefinitionList |> List.map Tuple.second


lookupForDefinition : String -> List ( String, Exposed ) -> Maybe Exposed
lookupForDefinition key =
    List.filter (Tuple.first >> (==) key) >> List.head >> Maybe.map Tuple.second


buildInterfaceFromExplicit : List (Ranged TopLevelExpose) -> List ( String, Exposed ) -> Interface
buildInterfaceFromExplicit x fileDefinitionList =
    x
        |> List.filterMap
            (\( _, expose ) ->
                case expose of
                    InfixExpose k ->
                        lookupForDefinition k fileDefinitionList

                    TypeOrAliasExpose s ->
                        lookupForDefinition s fileDefinitionList
                            |> Maybe.map (ifType (\( name, _ ) -> Type ( name, [] )))

                    FunctionExpose s ->
                        Just <| Function s

                    TypeExpose exposedType ->
                        case exposedType.open of
                            Nothing ->
                                Just <| Type ( exposedType.name, [] )

                            Just _ ->
                                lookupForDefinition exposedType.name fileDefinitionList
            )


ifType : (( String, List String ) -> Exposed) -> Exposed -> Exposed
ifType f i =
    case i of
        Type t ->
            f t

        _ ->
            i


fileToDefinitions : File -> List ( String, Exposed )
fileToDefinitions file =
    let
        allDeclarations =
            file.declarations
                |> List.filterMap
                    (\( _, decl ) ->
                        case decl of
                            TypeDecl t ->
                                Just ( t.name, Type ( t.name, t.constructors |> List.map .name ) )

                            AliasDecl a ->
                                Just ( a.name, Alias a.name )

                            PortDeclaration p ->
                                Just ( p.name.value, Function p.name.value )

                            FuncDecl f ->
                                Just ( f.declaration.name.value, Function f.declaration.name.value )

                            InfixDeclaration i ->
                                Just ( Tuple.second i.operator, Operator i )

                            Destructuring _ _ ->
                                Nothing
                    )

        getValidOperatorInterface : Exposed -> Exposed -> Maybe Exposed
        getValidOperatorInterface t1 t2 =
            case ( t1, t2 ) of
                ( Operator x, Operator y ) ->
                    if Tuple.second x.precedence == 5 && Tuple.second x.direction == Left then
                        Just <| Operator y

                    else
                        Just <| Operator x

                _ ->
                    Nothing

        resolveGroup g =
            case g of
                [] ->
                    Nothing

                [ x ] ->
                    Just x

                [ ( n1, t1 ), ( _, t2 ) ] ->
                    getValidOperatorInterface t1 t2
                        |> Maybe.map (\a -> ( n1, a ))

                _ ->
                    Nothing
    in
    allDeclarations
        |> List.map Tuple.first
        |> List.Extra.unique
        |> List.map
            (\x ->
                ( x
                , allDeclarations
                    |> List.filter (Tuple.first >> (==) x)
                )
            )
        |> List.filterMap (Tuple.second >> resolveGroup)
