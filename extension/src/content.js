import { parser, TIMELINE_SELECTOR, SIDEBAR_SELECTOR, DEBUG } from "parser";

let running = false;
const sendAds = function() {
  if (running) return;
  if (DEBUG) console.log("sending Ads");
  running = true;
  let posts = Array.from(document.querySelectorAll(SIDEBAR_SELECTOR)).concat(
    Array.from(document.querySelectorAll(TIMELINE_SELECTOR))
  );

  let results = [];
  let scraper = posts.reduce(
    (p, i) =>
      p.then(() =>
        parser(i).then(
          it => {
            results.push(it);
          },
          e => {
            if (DEBUG) console.log(e);
          }
        )
      ),
    Promise.resolve(null)
  );

  scraper.then(() => {
    if (DEBUG) console.log("sending these: ", results);
    chrome.runtime.sendMessage(results);
    if (DEBUG) console.log("done sending Ads, sent", results.length);
    running = false;
  });
};

setInterval(sendAds, 10000);
