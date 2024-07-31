#!/usr/bin/env node

const fs = require('node:fs');
const publishedElm = require('./published/elm.js');
const currentElm = require('./current/elm.js');

const fileToParses = process.argv.slice(2);

console.log(publishedElm);
const published = publishedElm.Elm.ParseMain.init();
const current = currentElm.Elm.ParseMain.init();

(async function() {
    for await (const filePath of fileToParses) {
        //const publishedName = 'published ' + filePath;
        //const currentName = 'current ' + filePath;
        //console.time(publishedName)
        //console.timeEnd(publishedName)
        //console.time(currentName)
        //console.timeEnd(currentName)
        const content = fs.readFileSync(filePath, 'utf8');
        const before = await parse(published, "published", content)
        const after = await parse(current, "current", content)
    }
})()

function parse(elmApp, name, content) {
    return new Promise((resolve) => {
      function fn(data) {
        elmApp.ports.parseResult.unsubscribe(fn);
        resolve(data);
      }
      elmApp.ports.parseResult.subscribe(fn);
      elmApp.ports.requestParsing.send(content);
    });
}
