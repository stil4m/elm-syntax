#!/bin/bash

cd find-regressions/current
elm make --optimize ../src/ParseMain.elm --output elm.js
sed -i 's/$author$project$ParseMain$timeStart(version)/globalThis.performance.now()/' elm.js
sed -i 's/$author$project$ParseMain$timeEnd(version)/globalThis.measurements[version].push(globalThis.performance.now() - start)/' elm.js

cd ../..
cd find-regressions/published
elm make --optimize ../src/ParseMain.elm --output elm.js
sed -i 's/$author$project$ParseMain$timeStart(version)/globalThis.performance.now()/' elm.js
sed -i 's/$author$project$ParseMain$timeEnd(version)/globalThis.measurements[version].push(globalThis.performance.now() - start)/' elm.js
