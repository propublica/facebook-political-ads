import { parser, TIMELINE_SELECTOR, SIDEBAR_SELECTOR, DEBUG } from "parser";
import debounce from "lodash/debounce";

let running = false;
const sendAds = function() {
  if (running) return;
  console.log("sending Ads");
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
    console.log(results);
    chrome.runtime.sendMessage(results);
    console.log("done sending Ads");
    running = false;
  });
};

setInterval(sendAds, 10000);
