#!/bin/bash

FILES=$@
TARGET="tests/RegressionTests.elm"
BRANCH="pratt-parser"

mkdir -p regressions

for FILE in $FILES
do
    cat > $TARGET << EOF
module RegressionTests exposing (..)

import Elm.Parser
import Expect
import Test


test =
    Test.only <|
        Test.test "parse all" <|
            \() ->
                """
$(cat $FILE | sed 's/\\/\\\\/g' | sed 's/\"\"\"/\\"\\"\\"/g')
"""
    |> Elm.Parser.parseToFile
    |> Debug.log "__RESULT__"
    |> Expect.ok
EOF

    echo "Checking $FILE"

    git checkout master > /dev/null 2>&1
    npx elm-test | grep __RESULT__ > master_result.tmp

    git checkout $BRANCH > /dev/null 2>&1
    npx elm-test | grep __RESULT__ > branch_result.tmp

    diff master_result.tmp branch_result.tmp > /dev/null
    if [ $? -ne 0 ] ;then
        echo "❌❌❌ Found a parsing difference in $FILE"
        cp $FILE regressions
    else
       echo "✅✅✅ No problem"
    fi
done
