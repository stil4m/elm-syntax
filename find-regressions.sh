#!/bin/bash

# # Instructions
#
# Run the script like (from the root folder) :
#
#     ./find-regressions.sh some-folder/**\/*.elm
#
# The version it will compare to is the one in `find-regressions/published/elm.json`
# under the `dependencies.direct.['stil4m/elm-syntax']` field.
#
# The script will also give performance metrics.
# If you only want to get that (and don't care about the regression check), run:
#
#     ./find-regressions.sh --no-check some-folder/**\/*.elm
#

cd find-regressions/current
elm make --optimize ../src/ParseMain.elm --output elm.js > /dev/null
sed -i 's/$author$project$ParseMain$timeStart(version)/globalThis.performance.now()/' elm.js
sed -i 's/$author$project$ParseMain$timeEnd(version)/globalThis.measurements[version].push(globalThis.performance.now() - start)/' elm.js

cd ../..
cd find-regressions/published
elm make --optimize ../src/ParseMain.elm --output elm.js > /dev/null
sed -i 's/$author$project$ParseMain$timeStart(version)/globalThis.performance.now()/' elm.js
sed -i 's/$author$project$ParseMain$timeEnd(version)/globalThis.measurements[version].push(globalThis.performance.now() - start)/' elm.js

cd ../..
node find-regressions $@