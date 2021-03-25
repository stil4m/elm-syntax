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
                                            , Node emptyRange <| Elm.Syntax.TypeAnnotation.Type (Node emptyRange <| ( [], "String" )) []
                                            )
                                        ]
                            }
                        )
        , test "type alias without spacings around '='" <|
            \() ->
                parseFullStringWithNullState "type alias Foo ={color: String }" Parser.typeDefinition
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
        , test "type alias with Var " <|
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
                                            , Node emptyRange <| Var "a"
                                            )
                                        ]
                            }
                        )
        , test "type alias extension record" <|
            \() ->
                parseFullStringWithNullState "type alias Foo abc = { abc |color: String }" Parser.typeDefinition
                    |> Maybe.andThen asTypeAlias
                    |> Maybe.map noRangeTypeAlias
                    |> Expect.equal
                        (Just <|
                            { documentation = Nothing
                            , name = Node emptyRange "Foo"
                            , generics = [ Node emptyRange "abc" ]
                            , typeAnnotation =
                                Node emptyRange <|
                                    ExtensionRecord
                                        (Node emptyRange "abc")
                                        (Node emptyRange <|
                                            ( Node emptyRange <| "color"
                                            , Node emptyRange <| Elm.Syntax.TypeAnnotation.Type (Node emptyRange <| ( [], "String" )) []
                                            )
                                        )
                                        []
                            }
                        )
        , test "type alias extension record with no fields fails" <|
            \() ->
                parseFullStringWithNullState "type alias Foo abc = { abc |  }" Parser.typeDefinition
                    |> Maybe.andThen asTypeAlias
                    |> Maybe.map noRangeTypeAlias
                    |> Expect.equal Nothing
        , test "type" <|
            \() ->
                parseFullStringWithNullState "type Color = Blue String | Red | Green" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just <|
                            { documentation = Nothing
                            , name = Node emptyRange <| "Color"
                            , generics = []
                            , firstConstructor =
                                Node emptyRange
                                    { name = Node emptyRange <| "Blue"
                                    , arguments = [ Node emptyRange <| Elm.Syntax.TypeAnnotation.Type (Node emptyRange <| ( [], "String" )) [] ]
                                    }
                            , restOfConstructors =
                                [ Node emptyRange
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
                            { documentation = Nothing
                            , firstConstructor =
                                Node emptyRange
                                    { arguments =
                                        [ Node emptyRange <| Var "a"
                                        , Node emptyRange <| Elm.Syntax.TypeAnnotation.Type (Node emptyRange <| ( [], "B" )) []
                                        ]
                                    , name = Node emptyRange <| "C"
                                    }
                            , restOfConstructors = []
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
                            { documentation = Nothing
                            , firstConstructor =
                                Node emptyRange
                                    { arguments =
                                        [ Node emptyRange <| Elm.Syntax.TypeAnnotation.Type (Node emptyRange <| ( [], "B" )) []
                                        , Node emptyRange <| Var "a"
                                        ]
                                    , name = Node emptyRange <| "C"
                                    }
                            , restOfConstructors = []
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
                            { documentation = Nothing
                            , firstConstructor =
                                Node emptyRange
                                    { arguments =
                                        [ Node emptyRange <| Elm.Syntax.TypeAnnotation.Type (Node emptyRange <| ( [], "B" )) []
                                        ]
                                    , name = Node emptyRange <| "C"
                                    }
                            , restOfConstructors = []
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
                            { documentation = Nothing
                            , firstConstructor =
                                Node emptyRange
                                    { arguments = []
                                    , name = Node emptyRange <| "Blue"
                                    }
                            , restOfConstructors = []
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
                            { documentation = Nothing
                            , name = Node emptyRange <| "Maybe"
                            , generics = [ Node emptyRange <| "a" ]
                            , firstConstructor =
                                Node emptyRange
                                    { name = Node emptyRange <| "Just"
                                    , arguments = [ Node emptyRange <| Var "a" ]
                                    }
                            , restOfConstructors =
                                [ Node emptyRange
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
        , test "type with spacing after " <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "type A = B\n\n" Parser.typeDefinition
                    |> Expect.equal
                        (Just
                            (DefinedType { end = { column = 11, row = 1 }, start = { column = 1, row = 1 } }
                                { firstConstructor =
                                    Node { end = { column = 11, row = 1 }, start = { column = 10, row = 1 } }
                                        { arguments = [], name = Node { end = { column = 11, row = 1 }, start = { column = 10, row = 1 } } "B" }
                                , restOfConstructors = []
                                , documentation = Nothing
                                , generics = []
                                , name = Node { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } } "A"
                                }
                            )
                        )
        ]
