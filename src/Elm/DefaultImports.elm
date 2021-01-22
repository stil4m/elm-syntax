module Elm.DefaultImports exposing (defaults)

import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range


defaults : List Import
defaults =
    [ { moduleName = Node Range.emptyRange <| [ "Basics" ]
      , exposingList = Just (Node Range.emptyRange <| All Range.emptyRange)
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "List" ]
      , exposingList =
            Just <|
                Node Range.emptyRange <|
                    Explicit
                        [ Node Range.emptyRange <| TypeExpose (ExposedType "List" Nothing)
                        , Node Range.emptyRange <| InfixExpose "::"
                        ]
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "Maybe" ]
      , exposingList =
            Just <|
                Node Range.emptyRange <|
                    Explicit
                        [ Node Range.emptyRange <|
                            TypeExpose
                                (ExposedType "Maybe" (Just Range.emptyRange))
                        ]
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "Result" ]
      , exposingList =
            Just <|
                Node Range.emptyRange <|
                    Explicit
                        [ Node Range.emptyRange <|
                            TypeExpose
                                (ExposedType "Result" (Just Range.emptyRange))
                        ]
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "String" ], exposingList = Nothing, moduleAlias = Nothing }
    , { moduleName = Node Range.emptyRange <| [ "Tuple" ], exposingList = Nothing, moduleAlias = Nothing }
    , { moduleName = Node Range.emptyRange <| [ "Debug" ], exposingList = Nothing, moduleAlias = Nothing }
    , { moduleName = Node Range.emptyRange <| [ "Platform" ]
      , exposingList =
            Just <|
                Node Range.emptyRange <|
                    Explicit
                        [ Node Range.emptyRange <| TypeExpose (ExposedType "Program" Nothing)
                        ]
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "Platform", "Cmd" ]
      , exposingList =
            Just <|
                Node Range.emptyRange <|
                    Explicit
                        [ Node Range.emptyRange <| TypeExpose (ExposedType "Cmd" Nothing)
                        , Node Range.emptyRange <| InfixExpose "!"
                        ]
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "Platform", "Sub" ]
      , exposingList = Just <| Node Range.emptyRange <| Explicit [ Node Range.emptyRange <| TypeExpose (ExposedType "Sub" Nothing) ]
      , moduleAlias = Nothing
      }
    ]
