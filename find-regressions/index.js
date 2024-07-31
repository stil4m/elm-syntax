#!/usr/bin/env node

const fs = require('node:fs/promises');
const publishedElm = require('./published/elm.js');
const currentElm = require('./current/elm.js');

const fileToParse = process.argv[2];

(async function () {
    console.log(fileToParse);
    const content = await fs.readFile(fileToParse, 'utf8')
    console.log(content)
})()

