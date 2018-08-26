module Elm.Parser.TypingsTests exposing (all, asType, asTypeAlias)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Parser.Typings as Parser exposing (TypeDefinition(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (..)


asType : TypeDefinition -> Maybe Type
asType td =
    case td of
        DefinedType range t ->
            Just t

        _ ->
            Nothing


asTypeAlias : TypeDefinition -> Maybe TypeAlias
asTypeAlias td =
    case td of
        DefinedAlias r t ->
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
                            , name = Node emptyRange "Foo"
                            , generics = []
                            , typeAnnotation =
                                Node emptyRange <|
                                    Record
                                        [ Node emptyRange <|
                                            ( Node emptyRange <| "color"
                                            , Node emptyRange <| Typed (Node emptyRange <| ( [], "String" )) []
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
                            , name = Node emptyRange <| "Foo"
                            , generics = [ Node emptyRange <| "a" ]
                            , typeAnnotation =
                                Node emptyRange <|
                                    Record
                                        [ Node emptyRange <|
                                            ( Node emptyRange <| "some"
                                            , Node emptyRange <| GenericType "a"
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
                            { name = Node emptyRange <| "Color"
                            , generics = []
                            , constructors =
                                [ Node emptyRange
                                    { name = Node emptyRange <| "Blue"
                                    , arguments = [ Node emptyRange <| Typed (Node emptyRange <| ( [], "String" )) [] ]
                                    }
                                , Node emptyRange
                                    { name = Node emptyRange <| "Red"
                                    , arguments = []
                                    }
                                , Node emptyRange
                                    { name = Node emptyRange <| "Green"
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
                            { constructors =
                                [ Node emptyRange
                                    { arguments =
                                        [ Node emptyRange <| GenericType "a"
                                        , Node emptyRange <| Typed (Node emptyRange <| ( [], "B" )) []
                                        ]
                                    , name = Node emptyRange <| "C"
                                    }
                                ]
                            , generics = []
                            , name = Node emptyRange <| "D"
                            }
                        )
        , test "type with multiple args and correct distribution of args" <|
            \() ->
                parseFullStringWithNullState "type D = C B a" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just
                            { constructors =
                                [ Node emptyRange
                                    { arguments =
                                        [ Node emptyRange <| Typed (Node emptyRange <| ( [], "B" )) []
                                        , Node emptyRange <| GenericType "a"
                                        ]
                                    , name = Node emptyRange <| "C"
                                    }
                                ]
                            , generics = []
                            , name = Node emptyRange <| "D"
                            }
                        )
        , test "type args should not continue on next line" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "type D = C B\na" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just
                            { constructors =
                                [ Node emptyRange
                                    { arguments =
                                        [ Node emptyRange <| Typed (Node emptyRange <| ( [], "B" )) []
                                        ]
                                    , name = Node emptyRange <| "C"
                                    }
                                ]
                            , generics = []
                            , name = Node emptyRange <| "D"
                            }
                        )
        , test "type and more" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "type Color = Blue \nsomethingElse = 1" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just
                            { constructors =
                                [ Node emptyRange
                                    { arguments = []
                                    , name = Node emptyRange <| "Blue"
                                    }
                                ]
                            , generics = []
                            , name = Node emptyRange <| "Color"
                            }
                        )
        , test "type with GenericType" <|
            \() ->
                parseFullStringWithNullState "type Maybe a = Just a | Nothing" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just <|
                            { name = Node emptyRange <| "Maybe"
                            , generics = [ Node emptyRange <| "a" ]
                            , constructors =
                                [ Node emptyRange
                                    { name = Node emptyRange <| "Just"
                                    , arguments = [ Node emptyRange <| GenericType "a" ]
                                    }
                                , Node emptyRange
                                    { name = Node emptyRange <| "Nothing"
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
        ]
