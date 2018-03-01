import { parser, TIMELINE_SELECTOR, SIDEBAR_SELECTOR } from "parser";
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
      p.then(() => {
        let timeout = new Promise(resolve =>
          setTimeout(() => resolve(false), 5000)
        );
        return Promise.race([
          parser(i).then(it => results.push(it), e => console.log(e)),
          timeout
        ]);
      }),
    Promise.resolve(null)
  );

  scraper.then(() => {
    chrome.runtime.sendMessage(results.filter(i => i));
    console.log("done sending Ads");

    running = false;
  });
};

let a = new MutationObserver(debounce(sendAds, 5000));
a.observe(document.body, { childList: true, subtree: true });
