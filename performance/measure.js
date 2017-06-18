const Elm = require("./performance");
const fs = require("fs");

const files = [
    // [
    //     "Basics.elm",
    //     fs
    //         .readFileSync(
    //             "elm-stuff/packages/elm-lang/core/5.1.1/src/Basics.elm"
    //         )
    //         .toString(),
    //     []
    // ],
    // [
    //     "String.elm",
    //     fs
    //         .readFileSync(
    //             "elm-stuff/packages/elm-lang/core/5.1.1/src/String.elm"
    //         )
    //         .toString(),
    //     []
    // ],
    // [
    //     "Maybe.elm",
    //     fs
    //         .readFileSync(
    //             "elm-stuff/packages/elm-lang/core/5.1.1/src/Maybe.elm"
    //         )
    //         .toString(),
    //     []
    // ]
    [
        "Dict.elm",
        fs
            .readFileSync("elm-stuff/packages/elm-lang/core/5.1.1/src/Dict.elm")
            .toString(),
        []
    ]
];
const repeatPerFile = 20;
const app = Elm.Performance.worker();
var start = 0;
var end = 0;

const getNextFile = function() {
    return files.filter(x => x[2].length < repeatPerFile)[0];
};

function avg(values) {
    var total = 0;
    values.forEach(x => (total = total + x));
    return total / values.length;
}

function printResults() {
    files.forEach(f => {
        console.log(f[0], avg(f[2]));
    });
}

function addNext() {
    const nextFile = getNextFile();
    if (!nextFile) {
        printResults();
        return;
    }
    start = new Date().getTime();

    app.ports.onContent.send([nextFile[0], nextFile[1]]);
    console.log("Did send:", nextFile[0]);
}

app.ports.pushResult.subscribe(function() {
    end = new Date().getTime();
    getNextFile()[2].push(end - start);
    addNext();
});
addNext();
