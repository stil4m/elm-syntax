module Elm.DefaultImports exposing (defaults)

import Elm.Syntax.Exposing exposing (Exposing(All, None, Explicit), TopLevelExpose(TypeExpose, InfixExpose), ExposedType)
import Elm.Syntax.Module exposing (Import)
import Elm.Syntax.Range as Range


defaults : List Import
defaults =
    [ { moduleName = [ "Basics" ]
      , exposingList = All Range.emptyRange
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "List" ]
      , exposingList =
            Explicit
                [ TypeExpose (ExposedType "List" None Range.emptyRange)
                , InfixExpose "::" Range.emptyRange
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Maybe" ]
      , exposingList =
            Explicit
                [ TypeExpose
                    (ExposedType "Maybe"
                        (Explicit
                            [ ( "Just", Range.emptyRange )
                            , ( "Nothing", Range.emptyRange )
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
            Explicit
                [ TypeExpose
                    (ExposedType "Result"
                        (Explicit
                            [ ( "Ok", Range.emptyRange )
                            , ( "Err", Range.emptyRange )
                            ]
                        )
                        Range.emptyRange
                    )
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "String" ], exposingList = None, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Tuple" ], exposingList = None, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Debug" ], exposingList = None, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Platform" ]
      , exposingList =
            Explicit
                [ TypeExpose (ExposedType "Program" None Range.emptyRange)
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , exposingList =
            Explicit
                [ TypeExpose (ExposedType "Cmd" None Range.emptyRange)
                , InfixExpose "!" Range.emptyRange
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Platform", "Sub" ]
      , exposingList =
            Explicit
                [ TypeExpose (ExposedType "Sub" None Range.emptyRange)
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    ]
