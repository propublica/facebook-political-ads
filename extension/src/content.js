import { parser, TIMELINE_SELECTOR, SIDEBAR_SELECTOR } from 'parser';
import throttle from 'lodash/throttle';

let running = false;
const sendAds = function() {
  if(running) return;
  running = true;
  let posts = Array.from(document.querySelectorAll(SIDEBAR_SELECTOR))
    .concat(Array.from(document.querySelectorAll(TIMELINE_SELECTOR)));
  let results = [];
  let scraper = posts.reduce((p, i) => p.then(() => {
    let timeout = new Promise((resolve) => setTimeout(() => resolve(false), 10000));
    return Promise.race([
      parser(i).then(
        (it) => results.push(it),
        (e) => console.log(e)
      ),
      timeout
    ]);
  }), Promise.resolve(null));

  scraper.then(() => {
    chrome.runtime.sendMessage(results.filter((i) => i));
    running = false;
  });
};

let a = new MutationObserver(throttle(sendAds, 1000));
a.observe(document.body, {childList: true, subtree:true});
