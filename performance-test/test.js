const Elm = require("./performance");
const fs = require("fs-extra");
const app = Elm.Main.worker();

const contents = require('./input');
const _ = require('lodash');

var durations = [];
var start;

function report() {
  console.log("Average:", _.meanBy(durations));
}
function send() {
  console.log(contents.length);
  const next = contents.shift();
  if (!next) {
    report();
    return;
  }
  start = new Date().getTime();
  app.ports.onFile.send(next);
}

app.ports.failedParse.subscribe(function() {
  console.log("Fail");
});

app.ports.successParse.subscribe(function() {
  var duration = new Date().getTime() - start;
  durations.push(duration);
  console.log("Success");
  send();
});

send();
