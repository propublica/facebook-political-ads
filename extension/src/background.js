import {
  sendAds,
  mergeAds,
  updateBadge,
  adForRequest,
  getBrowserLocale,
  addYgidToAd
} from "utils.js";

chrome.runtime.onMessage.addListener(({ type, ads, ygid }) => {
  if (!localStorage.getItem("redux")) return;
  const store = JSON.parse(localStorage.getItem("redux"));
  if (type == "ads") {
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
    const store = JSON.parse(localStorage.getItem("redux"));
    store.ygid = ygid;
    localStorage.setItem("redux", JSON.stringify(store));
  } else {
    console.log("got unknown msg of type ", type);
  }
});

chrome.browserAction.setBadgeBackgroundColor({ color: "#0099E6" });
