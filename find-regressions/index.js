#!/usr/bin/env node

// Run this through find-regressions.sh at the root of the project.
// More instructions on how to run the script is available in that file.

const fs = require('node:fs');
try {
    require('./published/elm.js');
    require('./current/elm.js');
} catch (error) {
    console.error(`ERROR: You have not compiled the Elm applications yet.

Please run this script through "find-regressions.sh".
`)
    process.exit(1);
}
const publishedElm = require('./published/elm.js');
const currentElm = require('./current/elm.js');

const disableEquality = process.argv.includes('--no-check');
const fileToParses = process.argv.slice(2).filter(arg => arg !== '--no-check');

const published = publishedElm.Elm.ParseMain.init({ flags: 'published' });
const current = currentElm.Elm.ParseMain.init({ flags: 'current' });

globalThis.measurements = {
    'current': [],
    'published': [],
};

(async function() {
    for await (const filePath of fileToParses) {
        const source = fs.readFileSync(filePath, 'utf8');
        const before = await parse(published, source)
            .catch(error => {
                console.error(`Failure parsing ${filePath} with PUBLISHED`, error);
                return 'FAILURE';
            })
        const after = await parse(current, source)
            .catch(error => {
                console.error(`Failure parsing ${filePath} with CURRENT`, error);
                return 'FAILURE';
            })
        if (!disableEquality) {
            if (JSON.stringify(before) !== JSON.stringify(after)) {
                console.error(`DIFFERENT: ${filePath}`);
            }
        }
    }

    const publishedAvg = avg(globalThis.measurements.published);
    const currentAvg = avg(globalThis.measurements.current);
    const speedup = (publishedAvg / currentAvg - 1) * 100
    console.log('Published: avg ' + publishedAvg);
    console.log('Current:   avg ' + currentAvg);
    console.log(`Diff: ${speedup}% faster`);
})()

function parse(elmApp, source) {
    return new Promise((resolve) => {
      elmApp.ports.parseResult.subscribe(fn);
      elmApp.ports.requestParsing.send(source);
      function fn(data) {
        elmApp.ports.parseResult.unsubscribe(fn);
        resolve(data);
      }
    });
}


function avg(array) {
    return array.reduce((a, b) => a + b, 0) / array.length;
}