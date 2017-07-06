import parser from "parser";

let sendAds = function(){
  let posts = Array.from(document.querySelectorAll(".fbUserContent"));
  let sponsor = chrome.i18n.getMessage("sponsor_text");
  let ads = posts.map((i) =>  parser(i, sponsor)).filter((i) => i);
  chrome.runtime.sendMessage(ads);
};

let observer = new MutationObserver(sendAds);
observer.observe(document.querySelector("body"), {childList: true});
