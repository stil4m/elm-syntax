#!/usr/bin/env node

const fs = require('node:fs');
const publishedElm = require('./published/elm.js');
const currentElm = require('./current/elm.js');

const checkEquality = process.argv[2] === '--check';
const fileToParses = process.argv.slice(checkEquality ? 3 : 2);

const published = publishedElm.Elm.ParseMain.init();
const current = currentElm.Elm.ParseMain.init();

const publishedTimes = [];
const currentTimes = [];
(async function() {
    for await (const filePath of fileToParses) {
        const content = fs.readFileSync(filePath, 'utf8');
        const before = await parse(published, 'published ' + filePath, publishedTimes, content)
            .catch(error => {
                console.error(`Failure parsing ${filePath} with PUBLISHED`, error);
                return "FAILURE";
            })
        const after = await parse(current, 'current ' + filePath, currentTimes, content)
            .catch(error => {
                console.error(`Failure parsing ${filePath} with CURRENT`, error);
                return "FAILURE";
            })
        if (checkEquality) {
            if (JSON.stringify(before) !== JSON.stringify(after)) {
                console.error(`DIFFERENT: ${filePath}`);
            }
        }
    }

    const publishedAvg = avg(publishedTimes);
    const currentAvg = avg(currentTimes);
    const speedup = (publishedAvg / currentAvg - 1) * 100
    console.log('Published: avg ' + publishedAvg);
    console.log('Current:   avg ' + currentAvg);
    console.log(`Diff: ${speedup}% faster`);
})()

function parse(elmApp, name, array, content) {
    return new Promise((resolve) => {
      elmApp.ports.parseResult.subscribe(fn);
      const startTime = globalThis.performance.now();
      elmApp.ports.requestParsing.send(content);
      function fn(data) {
        array.push(globalThis.performance.now() - startTime);
        elmApp.ports.parseResult.unsubscribe(fn);
        resolve(data);
      }
    });
}


function avg(array) {
    return array.reduce((a, b) => a + b, 0) / array.length;
}