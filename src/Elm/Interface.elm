module Elm.Interface exposing (..)

import Elm.Syntax.File as AST
import Elm.Syntax.Infix as AST exposing (Infix)
import Elm.Syntax.Module as Module
import Elm.Syntax.Exposing as AST
import Elm.Syntax.Declaration as AST
import List.Extra


type alias Interface =
    List Exposed


type Exposed
    = Function String
    | Type ( String, List String )
    | Alias String
    | Operator Infix


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
                        inf.operator == k

                    Alias _ ->
                        False
            )


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


build : AST.File -> Interface
build file =
    let
        fileDefinitionList =
            fileToDefinitions file

        moduleExposing =
            Module.exposingList file.moduleDefinition
    in
        case moduleExposing of
            AST.None ->
                []

            AST.Explicit x ->
                buildInterfaceFromExplicit x fileDefinitionList

            AST.All _ ->
                fileDefinitionList |> List.map Tuple.second


lookupForDefinition : String -> List ( String, Exposed ) -> Maybe Exposed
lookupForDefinition key =
    List.filter (Tuple.first >> (==) key) >> List.head >> Maybe.map Tuple.second


buildInterfaceFromExplicit : List AST.TopLevelExpose -> List ( String, Exposed ) -> Interface
buildInterfaceFromExplicit x fileDefinitionList =
    x
        |> List.filterMap
            (\expose ->
                case expose of
                    AST.InfixExpose k _ ->
                        lookupForDefinition k fileDefinitionList

                    AST.TypeOrAliasExpose s _ ->
                        lookupForDefinition s fileDefinitionList
                            |> Maybe.map (ifType (\( name, _ ) -> Type ( name, [] )))

                    AST.FunctionExpose s _ ->
                        Just <| Function s

                    AST.TypeExpose exposedType ->
                        case exposedType.constructors of
                            AST.None ->
                                Just <| Type ( exposedType.name, [] )

                            AST.All _ ->
                                lookupForDefinition exposedType.name fileDefinitionList

                            AST.Explicit v ->
                                Just <| Type ( exposedType.name, List.map Tuple.first v )
            )


ifType : (( String, List String ) -> Exposed) -> Exposed -> Exposed
ifType f i =
    case i of
        Type t ->
            f t

        _ ->
            i


fileToDefinitions : AST.File -> List ( String, Exposed )
fileToDefinitions file =
    let
        allDeclarations =
            file.declarations
                |> List.filterMap
                    (\decl ->
                        case decl of
                            AST.TypeDecl t ->
                                Just ( t.name, Type ( t.name, t.constructors |> List.map .name ) )

                            AST.AliasDecl a ->
                                Just ( a.name, Alias a.name )

                            AST.PortDeclaration p ->
                                Just ( p.name, Function p.name )

                            AST.FuncDecl f ->
                                if f.declaration.operatorDefinition then
                                    Just
                                        ( f.declaration.name.value
                                        , Operator
                                            { operator = f.declaration.name.value
                                            , precedence = 5
                                            , direction = AST.Left
                                            }
                                        )
                                else
                                    Just ( f.declaration.name.value, Function f.declaration.name.value )

                            AST.InfixDeclaration i ->
                                Just ( i.operator, Operator i )

                            AST.Destructuring _ _ ->
                                Nothing
                    )

        getValidOperatorInterface : Exposed -> Exposed -> Maybe Exposed
        getValidOperatorInterface t1 t2 =
            case ( t1, t2 ) of
                ( Operator x, Operator y ) ->
                    if x.precedence == 5 && x.direction == AST.Left then
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
                        |> Maybe.map ((,) n1)

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
