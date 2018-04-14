const scanSync = require("scan-dir-recursive/sync");
const fs = require("fs-extra");
const result = scanSync("../src");
const _ = require('lodash');
var answer = [];

for (var i = 0; i < 5; i++) {
  result.map(f => {
    const fileContent = fs.readFileSync(f).toString();
    answer.push(fileContent);
  });
}

fs.writeFileSync('./input.json', JSON.stringify(answer));
