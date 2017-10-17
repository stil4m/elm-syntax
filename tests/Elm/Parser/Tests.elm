module Elm.Parser.Tests exposing (suite)

import Elm.Parser.CaseExpressionTests as CaseExpressionTests
import Elm.Parser.CommentTest as CommentTest
import Elm.Parser.DeclarationsTests as DeclarationsTests
import Elm.Parser.ExposeTests as ExposeTests
import Elm.Parser.ExpressionTests as ExpressionTests
import Elm.Parser.FileTests as FileTests
import Elm.Parser.GlslTests as GlslTests
import Elm.Parser.ImportsTests as ImportsTests
import Elm.Parser.InfixTests as InfixTests
import Elm.Parser.LambdaExpressionTests as LambdaExpressionTests
import Elm.Parser.LetExpressionTests as LetExpressionTests
import Elm.Parser.ModuleTests as ModuleTests
import Elm.Parser.PatternTests as PatternTests
import Elm.Parser.TokenTests as TokenTests
import Elm.Parser.TypeAnnotationTests as TypeAnnotationTests
import Elm.Parser.TypingsTests as TypingsTests
import Elm.Parser.UtilTests as UtilTests
import Test exposing (Test)


suite : Test
suite =
    Test.concat
        [ TokenTests.all
        , UtilTests.all
        , ModuleTests.all
        , ImportsTests.all
        , CommentTest.all
        , ExposeTests.all
        , TypingsTests.all
        , TypeAnnotationTests.all
        , InfixTests.all
        , GlslTests.all
        , ExpressionTests.all
        , LetExpressionTests.all
        , DeclarationsTests.all
        , CaseExpressionTests.all
        , LambdaExpressionTests.all
        , PatternTests.all
        , FileTests.all
        ]
