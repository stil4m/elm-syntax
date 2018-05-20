module Elm.Parser.TypingsTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Parser.Typings as Parser exposing (TypeDefinition(..))
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (..)


main =
    Tuple.second all


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
                            , name = "Foo"
                            , generics = []
                            , typeAnnotation = ( emptyRange, Record [ ( "color", ( emptyRange, Typed [] "String" [] ) ) ] )
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
                            , name = "Foo"
                            , generics = [ "a" ]
                            , typeAnnotation = ( emptyRange, Record [ ( "some", ( emptyRange, GenericType "a" ) ) ] )
                            }
                        )
        , test "type" <|
            \() ->
                parseFullStringWithNullState "type Color = Blue String | Red | Green" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just
                            { name = "Color"
                            , generics = []
                            , constructors =
                                [ { name = "Blue"
                                  , arguments = [ ( emptyRange, Typed [] "String" [] ) ]
                                  , range = emptyRange
                                  }
                                , { name = "Red"
                                  , arguments = []
                                  , range = emptyRange
                                  }
                                , { name = "Green"
                                  , arguments = []
                                  , range = emptyRange
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
                                [ { arguments =
                                        [ ( emptyRange, GenericType "a" )
                                        , ( emptyRange, Typed [] "B" [] )
                                        ]
                                  , name = "C"
                                  , range = emptyRange
                                  }
                                ]
                            , generics = []
                            , name = "D"
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
                                [ { arguments =
                                        [ ( emptyRange, Typed [] "B" [] )
                                        , ( emptyRange, GenericType "a" )
                                        ]
                                  , name = "C"
                                  , range = emptyRange
                                  }
                                ]
                            , generics = []
                            , name = "D"
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
                                [ { arguments =
                                        [ ( emptyRange, Typed [] "B" [] )
                                        ]
                                  , name = "C"
                                  , range = emptyRange
                                  }
                                ]
                            , generics = []
                            , name = "D"
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
                                [ { arguments = []
                                  , name = "Blue"
                                  , range = emptyRange
                                  }
                                ]
                            , generics = []
                            , name = "Color"
                            }
                        )
        , test "type with GenericType" <|
            \() ->
                parseFullStringWithNullState "type Maybe a = Just a | Nothing" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Maybe.map noRangeTypeDeclaration
                    |> Expect.equal
                        (Just
                            { name = "Maybe"
                            , generics = [ "a" ]
                            , constructors =
                                [ { name = "Just"
                                  , arguments = [ ( emptyRange, GenericType "a" ) ]
                                  , range = emptyRange
                                  }
                                , { name = "Nothing", arguments = [], range = emptyRange }
                                ]
                            }
                        )
        , test "type with value on next line " <|
            \() ->
                parseFullStringWithNullState "type Maybe a = Just a |\nNothing" Parser.typeDefinition
                    |> Maybe.andThen asType
                    |> Expect.equal Nothing
        ]
