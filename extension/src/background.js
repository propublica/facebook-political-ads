import {
  sendAds,
  mergeAds,
  updateBadge,
  adForRequest,
  getBrowserLocale,
  addYgidToAd
} from "utils.js";

chrome.runtime.onMessage.addListener(({ type, ads, ygid }) => {
  if (type == "ads") {
    if (!localStorage.getItem("redux")) return; // localStorage.getItem("redux") is ordinarily set by the popup's integration with redux-localstorage
    const store = JSON.parse(localStorage.getItem("redux"));
    try {
      if (!store.terms) return;
      let saved = new Set();
      store.ratings.map(ad => saved.add(ad.id));
      store.ratings = mergeAds(store.ratings || [], ads);
      localStorage.setItem("redux", JSON.stringify(store));
      updateBadge(store.ratings);
      const saving = ads
        .filter(ad => !saved.has(ad.id))
        .map(adForRequest)
        .map(addYgidToAd(store.ygid));
      const success = () => console.log("saved");
      const failure = e => console.log(e);
      if (saving.length > 0) {
        sendAds(saving, store.language || getBrowserLocale()).then(
          success,
          failure
        );
      }
    } catch (e) {
      console.log(e);
    }
  } else if (type == "ygid") {
    const store = JSON.parse(localStorage.getItem("redux")) || {};
    store.ygid = ygid;
    localStorage.setItem("redux", JSON.stringify(store));
    updateBadgeForYougovId(store);
  } else {
    console.log("got unknown msg of type ", type);
  }
});

chrome.browserAction.setBadgeBackgroundColor({ color: "#0099E6" });

// for yougov, if yougov page is already open, we have to inject the yg.js content script manually.
chrome.tabs.query({ url: "*://*.yougov.com/*" }, yg_tabs => {
  yg_tabs.forEach(tab => {
    chrome.tabs.executeScript(tab.id, { file: "yg.js" });
  });
});

function updateBadgeForYougovId(store) {
  if (store && store.ygid && store.terms) {
    chrome.browserAction.setBadgeText({ text: "" });
    chrome.browserAction.setBadgeBackgroundColor({ color: "#0099E6" });
  } else {
    chrome.browserAction.setBadgeText({ text: "!" });
    chrome.browserAction.setBadgeBackgroundColor({ color: "#ff0000" });
  }
}

setTimeout(() => {
  let store = JSON.parse(localStorage.getItem("redux"));
  updateBadgeForYougovId(store);
}, 10);
