module Elm.Interface exposing
    ( Interface, Exposed(..)
    , build, exposesAlias, exposesFunction, operators
    )

{-|


# Elm.Interface

A type that represents the interface for an Elm module.
You can see this as a trimmed down version for of a file that only caontains the header (`module X exposing (..)`) and some small set of additional data.


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
import Elm.Syntax.Node as Node exposing (Node(..))
import List.Extra


{-| An interface is just a list of 'things' that are exposed by a module.

    [ Type "Color" [ "Red", "Blue" ], Function "asRgb" ]

-}
type alias Interface =
    List Exposed


{-| Union type for the things that a module can expose. These are `Function`s, `CustomType`s, and `Alias`es.

Elm core packages can also define `Operator`s, and thus we take that into account as well.
The `Infix` type alias will contain all the information regarding the operator

-}
type Exposed
    = Function String
    | CustomType ( String, List String )
    | Alias String
    | Operator Infix


{-| A function to check whether an `Interface` exposes an certain type alias.
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


{-| Check whether an `Interface` exposes an function.

    exposesFunction "A" [ Function "A", CustomType "B", [ "C" ] ] == True
    exposesFunction "B" [ Function "A", CustomType "B", [ "C" ] ] == False
    exposesFunction "<" [ Infix { operator = "<" , ... } ] == True
    exposesFunction "A" [ Alias "A" ] == False

-}
exposesFunction : String -> Interface -> Bool
exposesFunction k interface =
    interface
        |> List.any
            (\x ->
                case x of
                    Function l ->
                        k == l

                    CustomType ( _, constructors ) ->
                        List.member k constructors

                    Operator inf ->
                        Node.value inf.operator == k

                    Alias _ ->
                        False
            )


{-| Retrieve all operators exposed by the `Interface`
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
    in
    case Module.exposingList (Node.value file.moduleDefinition) of
        Explicit x ->
            buildInterfaceFromExplicit x fileDefinitionList

        All _ ->
            List.map Tuple.second fileDefinitionList


lookupForDefinition : String -> List ( String, Exposed ) -> Maybe Exposed
lookupForDefinition key =
    List.filter (Tuple.first >> (==) key) >> List.head >> Maybe.map Tuple.second


buildInterfaceFromExplicit : List (Node TopLevelExpose) -> List ( String, Exposed ) -> Interface
buildInterfaceFromExplicit x fileDefinitionList =
    x
        |> List.filterMap
            (\(Node _ expose) ->
                case expose of
                    InfixExpose k ->
                        lookupForDefinition k fileDefinitionList

                    TypeOrAliasExpose s ->
                        lookupForDefinition s fileDefinitionList
                            |> Maybe.map (ifCustomType (\( name, _ ) -> CustomType ( name, [] )))

                    FunctionExpose s ->
                        Just <| Function s

                    TypeExpose exposedType ->
                        case exposedType.open of
                            Nothing ->
                                Just <| CustomType ( exposedType.name, [] )

                            Just _ ->
                                lookupForDefinition exposedType.name fileDefinitionList
            )


ifCustomType : (( String, List String ) -> Exposed) -> Exposed -> Exposed
ifCustomType f i =
    case i of
        CustomType t ->
            f t

        _ ->
            i


fileToDefinitions : File -> List ( String, Exposed )
fileToDefinitions file =
    let
        allDeclarations =
            file.declarations
                |> List.filterMap
                    (\(Node _ decl) ->
                        case decl of
                            CustomTypeDeclaration t ->
                                Just ( Node.value t.name, CustomType ( Node.value t.name, t.constructors |> List.map (Node.value >> .name >> Node.value) ) )

                            AliasDeclaration a ->
                                Just ( Node.value a.name, Alias <| Node.value a.name )

                            PortDeclaration p ->
                                Just ( Node.value p.name, Function (Node.value p.name) )

                            FunctionDeclaration f ->
                                let
                                    declaration =
                                        Node.value f.declaration

                                    name =
                                        Node.value declaration.name
                                in
                                Just ( name, Function <| name )

                            InfixDeclaration i ->
                                Just ( Node.value i.operator, Operator i )

                            Destructuring _ _ ->
                                Nothing
                    )

        getValidOperatorInterface : Exposed -> Exposed -> Maybe Exposed
        getValidOperatorInterface t1 t2 =
            case ( t1, t2 ) of
                ( Operator x, Operator y ) ->
                    if Node.value x.precedence == 5 && Node.value x.direction == Left then
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
