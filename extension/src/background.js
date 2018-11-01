import {
  sendAds,
  mergeAds,
  updateBadge,
  adForRequest,
  getBrowserLocale
} from "utils.js";

chrome.runtime.onMessage.addListener(ads => {
  if (!localStorage.getItem("redux")) return;
  try {
    const store = JSON.parse(localStorage.getItem("redux"));
    if (!store.terms) return;
    let saved = new Set();
    store.ratings.map(ad => saved.add(ad.id));
    store.ratings = mergeAds(store.ratings || [], ads);
    localStorage.setItem("redux", JSON.stringify(store));
    updateBadge(store.ratings, store.stories_seen);
    const saving = ads.filter(ad => !saved.has(ad.id)).map(adForRequest);
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
});

chrome.browserAction.setBadgeBackgroundColor({ color: "#0099E6" });
