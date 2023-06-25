module Elm.Parser.TypingsTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Parser.Typings as Parser exposing (TypeDefinition(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (empty)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (..)


asType : TypeDefinition -> Maybe Type
asType td =
    case td of
        DefinedType _ t ->
            Just t

        _ ->
            Nothing


asTypeAlias : TypeDefinition -> Maybe TypeAlias
asTypeAlias td =
    case td of
        DefinedAlias _ t ->
            Just t

        _ ->
            Nothing


all : Test
all =
    describe "TypeAlias"
        [ test "type alias" <|
            \() ->
                parseFullStringWithNullState "type alias Foo = {color: String }" Parser.typeDefinition
                    |> Maybe.andThen asTypeAlias
                    |> Maybe.map noRangeTypeAlias
                    |> Expect.equal
                        (Just <|
                            { documentation = Nothing
                            , name = Node.empty "Foo"
                            , generics = []
                            , typeAnnotation =
                                Node.empty <|
                                    Record
                                        [ Node.empty <|
                                            ( Node.empty <| "color"
                                            , Node.empty <| Typed (Node.empty <| ( [], "String" )) []
                                            )
                                        ]
                            }
                        )
        , test "type alias without spacings around '='" <|
            \() ->
                parseFullStringWithNullState "type alias Foo={color: String }" Parser.typeDefinition
                    |> Maybe.andThen asTypeAlias
                    |> Maybe.map noRangeTypeAlias
                    |> Expect.equal
                        (Just <|
                            { documentation = Nothing
                            , name = Node.empty "Foo"
                            , generics = []
                            , typeAnnotation =
                                Node.empty <|
                                    Record
                                        [ Node.empty <|
                                            ( Node.empty <| "color"
                                            , Node.empty <| Typed (Node.empty <| ( [], "String" )) []
                                            )
                                        ]
                            }
                        )
        , test "type alias with GenericType " <|
            \() ->
                parseFullStringWithNullState "type alias Foo a = {some : a }" Parser.typeDefinition
                    |> Maybe.andThen asTypeAlias
                    |> Maybe.map noRangeTypeAlias
                    |> Expect.equal
                        (Just <|
                            { documentation = Nothing
                            , name = Node empty <| "Foo"
                            , generics = [ Node empty <| "a" ]
                            , typeAnnotation =
                                Node empty <|
                                    Record
                                        [ Node empty <|
                                            ( Node empty <| "some"
                                            , Node empty <| GenericType "a"
                                            )
                                        ]
                            }
                        )
        , test "type" <|
            \() ->
                parseFullStringWithNullState "type Color = Blue String | Red | Green" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just <|
                            { documentation = Nothing
                            , name = Node empty <| "Color"
                            , generics = []
                            , constructors =
                                [ Node empty
                                    { name = Node empty <| "Blue"
                                    , arguments = [ Node empty <| Typed (Node empty <| ( [], "String" )) [] ]
                                    }
                                , Node empty
                                    { name = Node empty <| "Red"
                                    , arguments = []
                                    }
                                , Node empty
                                    { name = Node empty <| "Green"
                                    , arguments = []
                                    }
                                ]
                            }
                        )
        , test "type with multiple args" <|
            \() ->
                parseFullStringWithNullState "type D = C a B" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just
                            { documentation = Nothing
                            , constructors =
                                [ Node empty
                                    { arguments =
                                        [ Node empty <| GenericType "a"
                                        , Node empty <| Typed (Node empty <| ( [], "B" )) []
                                        ]
                                    , name = Node empty <| "C"
                                    }
                                ]
                            , generics = []
                            , name = Node empty <| "D"
                            }
                        )
        , test "type with multiple args and correct distribution of args" <|
            \() ->
                parseFullStringWithNullState "type D = C B a" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just
                            { documentation = Nothing
                            , constructors =
                                [ Node empty
                                    { arguments =
                                        [ Node empty <| Typed (Node empty <| ( [], "B" )) []
                                        , Node empty <| GenericType "a"
                                        ]
                                    , name = Node empty <| "C"
                                    }
                                ]
                            , generics = []
                            , name = Node empty <| "D"
                            }
                        )
        , test "type args should not continue on next line" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "type D = C B\na" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just
                            { documentation = Nothing
                            , constructors =
                                [ Node empty
                                    { arguments =
                                        [ Node empty <| Typed (Node empty <| ( [], "B" )) []
                                        ]
                                    , name = Node empty <| "C"
                                    }
                                ]
                            , generics = []
                            , name = Node empty <| "D"
                            }
                        )
        , test "type and more" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "type Color = Blue \nsomethingElse = 1" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just
                            { documentation = Nothing
                            , constructors =
                                [ Node empty
                                    { arguments = []
                                    , name = Node empty <| "Blue"
                                    }
                                ]
                            , generics = []
                            , name = Node empty <| "Color"
                            }
                        )
        , test "type with GenericType" <|
            \() ->
                parseFullStringWithNullState "type Maybe a = Just a | Nothing" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just <|
                            { documentation = Nothing
                            , name = Node empty <| "Maybe"
                            , generics = [ Node empty <| "a" ]
                            , constructors =
                                [ Node empty
                                    { name = Node empty <| "Just"
                                    , arguments = [ Node empty <| GenericType "a" ]
                                    }
                                , Node empty
                                    { name = Node empty <| "Nothing"
                                    , arguments = []
                                    }
                                ]
                            }
                        )
        , test "type with value on next line " <|
            \() ->
                parseFullStringWithNullState "type Maybe a = Just a |\nNothing" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Expect.equal Nothing
        , test "type with spacing after " <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "type A = B\n\n" Parser.typeDefinition
                    |> Expect.equal
                        (Just
                            (DefinedType { end = { column = 11, row = 1 }, start = { column = 1, row = 1 } }
                                { constructors =
                                    [ Node { end = { column = 11, row = 1 }, start = { column = 10, row = 1 } }
                                        { arguments = [], name = Node { end = { column = 11, row = 1 }, start = { column = 10, row = 1 } } "B" }
                                    ]
                                , documentation = Nothing
                                , generics = []
                                , name = Node { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } } "A"
                                }
                            )
                        )
        ]
