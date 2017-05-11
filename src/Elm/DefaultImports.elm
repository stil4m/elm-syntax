module Elm.DefaultImports exposing (defaults)

import Elm.Syntax.Exposing as AST exposing (..)
import Elm.Syntax.Module as AST exposing (..)
import Elm.Syntax.Range as Range


defaults : List AST.Import
defaults =
    [ { moduleName = [ "Basics" ]
      , exposingList = AST.All Range.emptyRange
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "List" ]
      , exposingList =
            AST.Explicit
                [ TypeExpose (ExposedType "List" AST.None Range.emptyRange)
                , (InfixExpose "::" Range.emptyRange)
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Maybe" ]
      , exposingList =
            AST.Explicit
                [ TypeExpose
                    (ExposedType "Maybe"
                        (AST.Explicit
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
            AST.Explicit
                [ TypeExpose
                    (ExposedType "Result"
                        (AST.Explicit
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
    , { moduleName = [ "String" ], exposingList = AST.None, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Tuple" ], exposingList = AST.None, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Debug" ], exposingList = AST.None, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Platform" ]
      , exposingList =
            AST.Explicit
                [ TypeExpose (ExposedType "Program" AST.None Range.emptyRange)
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , exposingList =
            AST.Explicit
                [ TypeExpose (ExposedType "Cmd" AST.None Range.emptyRange)
                , (InfixExpose "!" Range.emptyRange)
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Platform", "Sub" ]
      , exposingList =
            AST.Explicit
                [ TypeExpose (ExposedType "Sub" AST.None Range.emptyRange)
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    ]
