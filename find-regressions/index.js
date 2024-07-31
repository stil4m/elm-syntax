#!/usr/bin/env node

const fs = require('node:fs');
const publishedElm = require('./published/elm.js');
const currentElm = require('./current/elm.js');

const fileToParses = process.argv.slice(2);

for (const filePath of fileToParses) {
    console.log(filePath);
    const content = fs.readFileSync(filePath, 'utf8')
    console.log(content)
}
