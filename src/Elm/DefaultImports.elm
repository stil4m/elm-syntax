module Elm.DefaultImports exposing (defaults)

import Elm.Syntax.Exposing exposing (ExposedType, Exposing(All, Explicit), TopLevelExpose(InfixExpose, TypeExpose))
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
                    [ TypeExpose (ExposedType "List" Nothing Range.emptyRange)
                    , InfixExpose "::" Range.emptyRange
                    ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Maybe" ]
      , exposingList =
            Just <|
                Explicit
                    [ TypeExpose
                        (ExposedType "Maybe"
                            (Just <|
                                Explicit
                                    [ ( Range.emptyRange, "Just" )
                                    , ( Range.emptyRange, "Nothing" )
                                    ]
                            )
                            Range.emptyRange
                        )
                    ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Result" ]
      , exposingList =
            Just <|
                Explicit
                    [ TypeExpose
                        (ExposedType "Result"
                            (Just <|
                                Explicit
                                    [ ( Range.emptyRange, "Ok" )
                                    , ( Range.emptyRange, "Err" )
                                    ]
                            )
                            Range.emptyRange
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
                    [ TypeExpose (ExposedType "Program" Nothing Range.emptyRange)
                    ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , exposingList =
            Just <|
                Explicit
                    [ TypeExpose (ExposedType "Cmd" Nothing Range.emptyRange)
                    , InfixExpose "!" Range.emptyRange
                    ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Platform", "Sub" ]
      , exposingList = Just <| Explicit [ TypeExpose (ExposedType "Sub" Nothing Range.emptyRange) ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    ]
