#!/usr/bin/env node

const fs = require('node:fs');
const publishedElm = require('./published/elm.js');
const currentElm = require('./current/elm.js');

const fileToParses = process.argv.slice(2);

console.log(publishedElm);
const published = publishedElm.Elm.ParseMain.init();
const current = currentElm.Elm.ParseMain.init();

for (const filePath of fileToParses) {
    //const publishedName = 'published ' + filePath;
    //const currentName = 'current ' + filePath;
    //console.time(publishedName)
    //console.timeEnd(publishedName)
    //console.time(currentName)
    //console.timeEnd(currentName)
    const content = fs.readFileSync(filePath, 'utf8');
    const promise1 = new Promise((resolve) => {
      function fn(data) {
        published.ports.parseResult.unsubscribe(fn);
        resolve(data);
      }
      published.ports.parseResult.subscribe(fn);
      published.ports.requestParsing.send(content);
    });
}
