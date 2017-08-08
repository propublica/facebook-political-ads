const JSDOM = require('jsdom').JSDOM;
const fs = require('fs');
const HTML = fs.readFileSync(__dirname + "/test.html");
const dom = new JSDOM(HTML);
const assert = require('assert');

// Not ideal, but we slot in document for the parsers use.
// It is the easiest way.
global.document = dom.window.document;

const parser = require('../src/parser.js');

let posts = Array.from(dom.window.document.querySelectorAll(".fbUserContent, .fbUserPost, ._5pcr"))
  .concat(Array.from(dom.window.document.querySelectorAll(".ego_unit")));
let ads = posts.map((i) => parser(i)).filter((i) => i);
console.log(ads)
assert.equal(ads.length, 8, "Found eight ads");
assert.equal(ads[0].id, "hyperfeed_story_id_5944756a18bdf2794599305", "Got an ad");
assert.equal(ads[1].id, "hyperfeed_story_id_594474fe329fb7047789758", "Found the other.");
