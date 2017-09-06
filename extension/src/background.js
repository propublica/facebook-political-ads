import { sendAds, mergeAds, updateBadge, adForRequest } from "utils.js";

chrome.runtime.onMessage.addListener((ads) => {
  if(!localStorage.getItem("redux")) return;
  try {
    const redux = JSON.parse(localStorage.getItem("redux"));
    if(!redux.terms) return;
  } catch(e) {
    return;
  }
  let saved = new Set();
  try {
    let store = JSON.parse(localStorage.getItem("redux"));
    store.ratings.map((ad) => saved.add(ad.id));
    store.ratings = mergeAds(store.ratings || [], ads);
    localStorage.setItem('redux', JSON.stringify(store));
    updateBadge(store.ratings);
  } catch(e) {
    console.log(e);
  }
  const saving = ads.filter((ad) => !saved.has(ad.id)).map(adForRequest);
  const success = () => console.log("saved");
  const failure = (e) => console.log(e);
  if(saving.length > 0) sendAds(saving).then(success, failure);
});
chrome.browserAction.setBadgeBackgroundColor({color: "#0099E6"});
