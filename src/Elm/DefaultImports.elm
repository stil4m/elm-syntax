module Elm.DefaultImports exposing (defaults)

import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Module exposing (Import)
import Elm.Syntax.Range as Range


defaults : List Import
defaults =
    [ { moduleName = [ "Basics" ]
      , exposingList = Just (All Range.emptyRange)
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "List" ]
      , exposingList =
            Just <|
                Explicit
                    [ ( Range.emptyRange, TypeExpose (ExposedType "List" Nothing) )
                    , ( Range.emptyRange, InfixExpose "::" )
                    ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Maybe" ]
      , exposingList =
            Just <|
                Explicit
                    [ ( Range.emptyRange
                      , TypeExpose
                            (ExposedType "Maybe" (Just Range.emptyRange))
                      )
                    ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Result" ]
      , exposingList =
            Just <|
                Explicit
                    [ ( Range.emptyRange
                      , TypeExpose
                            (ExposedType "Result" (Just Range.emptyRange))
                      )
                    ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "String" ], exposingList = Nothing, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Tuple" ], exposingList = Nothing, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Debug" ], exposingList = Nothing, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Platform" ]
      , exposingList =
            Just <|
                Explicit
                    [ ( Range.emptyRange, TypeExpose (ExposedType "Program" Nothing) )
                    ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , exposingList =
            Just <|
                Explicit
                    [ ( Range.emptyRange, TypeExpose (ExposedType "Cmd" Nothing) )
                    , ( Range.emptyRange, InfixExpose "!" )
                    ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Platform", "Sub" ]
      , exposingList = Just <| Explicit [ ( Range.emptyRange, TypeExpose (ExposedType "Sub" Nothing) ) ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    ]
