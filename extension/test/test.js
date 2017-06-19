const parser = require('../src/parser.js');
const JSDOM = require('jsdom').JSDOM;
const fs = require('fs');
const HTML = fs.readFileSync(__dirname + "/test.html");
const dom = new JSDOM(HTML);
const assert = require('assert');

let posts = Array.from(dom.window.document.querySelectorAll(".fbUserContent"));
let ads = posts.map((i) => parser(i, "Sponsored")).filter((i) => i);
assert.equal(ads.length, 2, "Found two ads");
assert.equal(ads[0].id, "hyperfeed_story_id_5944756a18bdf2794599305", "Got an ad");
assert.equal(ads[1].id, "hyperfeed_story_id_594474fe329fb7047789758", "Found the other.");
