import { parser, TIMELINE_SELECTOR, SIDEBAR_SELECTOR } from 'parser';
import throttle from 'lodash/throttle';

let running = false;
const sendAds = function() {
  if(running) return;
  running = true;
  let posts = Array.from(document.querySelectorAll(TIMELINE_SELECTOR))
    .concat(Array.from(document.querySelectorAll(SIDEBAR_SELECTOR)));
  let results = [];
  let scraper = posts.reduce((p, i) => p.then(() => {
    return parser(i).then((it) => results.push(it));
  }), Promise.resolve(null));
  let timeout = new Promise((resolve) => setTimeout(resolve, 5000));
  Promise.race([scraper, timeout]).then(() => {
    chrome.runtime.sendMessage(results.filter((i) => i));
    running = false;
  });
};

let a = new MutationObserver(throttle(sendAds, 5000));
a.observe(document.body, {childList: true, subtree:true});
