import parser from "parser";

let sendAds = function(){
  let posts = Array.from(document.querySelectorAll(".fbUserContent"));
  let ads = posts.map((i) => {
    return parser(i, chrome.i18n.getMessage("sponsor_text"));
  }).filter((i) => { return i; });
  chrome.runtime.sendMessage(ads, function() {
    console.log("sent");
  });
};

let observer = new MutationObserver(sendAds);
observer.observe(document.querySelector("body"));
