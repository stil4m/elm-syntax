import axios from "axios";
const { Elm } = require("./interface-generator");

type Files = { [key: string]: string };

async function generateDependencyString(name: string, version: string, files: Files): Promise<string> {
  return new Promise(accept => {
    const app = Elm.InterfaceGenerator.init({ flags: { files, name, version } });
    app.ports.onInterface.subscribe((v: any) => {
      accept(v);
    });
  });
}

async function loadFiles(name: string, version: string): Promise<Files> {
  const elmJson = (await axios.get(`https://raw.githubusercontent.com/${name}/${version}/elm.json`)).data;

  const modules: string[] = (Object.values(elmJson["exposed-modules"]) as string[][]).flat();

  const files: { [key: string]: string } = {};

  await Promise.all(
    modules.map(async mod => {
      const url = `https://raw.githubusercontent.com/${name}/${version}/src/${mod.replace(/\./g, "/")}.elm`;
      const { data } = await axios.get(url);
      files[mod] = data;
    })
  );
  return files;
}

async function generateDependency(name: string, version: string): Promise<string> {
  return generateDependencyString(name, version, await loadFiles(name, version));
}

(async function() {
  const elmCoreDependency = await generateDependency("elm/core", "1.0.5");
  const elmUrlDependency = await generateDependency("elm/url", "1.0.0");
  const elmParserDependency = await generateDependency("elm/parser", "1.1.0");

  console.log(`module Elm.WellKnownOperators exposing (wellKnownOperators)

import Dict
import Elm.Dependency exposing (Dependency)
import Elm.Interface exposing (Exposed(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))


wellKnownOperators : List Dependency
wellKnownOperators =
    [ elmCore, elmUrl, elmParser ]

elmCore : Dependency
elmCore =
    ${elmCoreDependency}


elmUrl : Dependency
elmUrl =
    ${elmUrlDependency}


elmParser : Dependency
elmParser =
    ${elmParserDependency}
`);
})();
