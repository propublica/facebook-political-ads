import { parser, TIMELINE_SELECTOR, SIDEBAR_SELECTOR } from 'parser';
import throttle from 'lodash/throttle';

let seen = new Set();
// our lock so we don't stomp all over facebook by overparsing.
let running = false;
const sendAds = function() {
  if(running) return;
  running = true;
  console.time('parsing');
  let posts = Array.from(document.querySelectorAll(TIMELINE_SELECTOR))
    .concat(Array.from(document.querySelectorAll(SIDEBAR_SELECTOR)));
  let results = [];
  posts.reduce((p, i) => p.then(() => {
    return parser(i, seen).then((it) => results.push(it));
  }), Promise.resolve(null)).then(() => {
    running = false;
    console.timeEnd('parsing');
    chrome.runtime.sendMessage(results.filter((i) => i));
  });
};

setInterval(sendAds, 1000);

if(document.readyState === 'complete')
  sendAds();
document.addEventListener('interactive', sendAds, false);
