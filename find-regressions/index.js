#!/usr/bin/env node

// Run this through find-regressions.sh at the root of the project.
// More instructions on how to run the script is available in that file.

const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const {spawn} = require('node:child_process');
const { fdir } = require('fdir');

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
const pathArguments = process.argv.slice(2).filter(arg => arg !== '--no-check');

const filesToParse =
    pathArguments.map(pathArgument => {
        if (pathArgument.endsWith(".elm")) {
            return [pathArgument];
        }
        return new fdir()
            .withFullPaths()
            .glob('./**/*.elm')
            .crawl(pathArgument)
            .sync();
    }).reduce((acc, array) => acc.concat(array));

const publishedApp = publishedElm.Elm.ParseMain.init({ flags: 'published' });
const currentApp = currentElm.Elm.ParseMain.init({ flags: 'current' });

globalThis.measurements = {
    'current': [],
    'published': [],
};

(async function() {
    const tmpDirectory = fs.mkdtempSync(path.join(os.tmpdir(), 'elm-syntax-regressions-'));

    for await (const filePath of filesToParse) {
        const source = fs.readFileSync(filePath, 'utf8');
        const published = await parse(publishedApp, source)
            .catch(error => {
                console.error(`Failure parsing ${filePath} with PUBLISHED`, error);
                return 'FAILURE';
            })
        const current = await parse(currentApp, source)
            .catch(error => {
                console.error(`Failure parsing ${filePath} with CURRENT`, error);
                return 'FAILURE';
            })
        if (!disableEquality) {
            if (JSON.stringify(published) !== JSON.stringify(current)) {
                const regressionFolder = path.join(tmpDirectory, path.basename(filePath, '.elm'));
                const pathPublished = path.join(regressionFolder, 'published.txt');
                const pathCurrent = path.join(regressionFolder, 'current.txt');
                fs.mkdirSync(path.dirname(pathPublished), {recursive: true});
                fs.writeFileSync(pathPublished, JSON.stringify(published, null, 4), {encoding: 'utf8'});
                fs.writeFileSync(pathCurrent, JSON.stringify(current, null, 4), {encoding: 'utf8'});
                fs.writeFileSync(path.join(regressionFolder, path.basename(filePath)), source, {encoding: 'utf8'});
                const diff = fs.createWriteStream(path.join(regressionFolder, 'diff.diff'), { flags: 'a' });
                diff.on('open', () => {
                    spawn(
                        "diff",
                        [pathPublished, pathCurrent],
                        { stdio: [diff, diff, diff] }
                    );
                });

                console.error(`DIFFERENT: ${filePath}`);
                console.error(`  Folder: ${path.dirname(pathPublished)}`);
                console.error()
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