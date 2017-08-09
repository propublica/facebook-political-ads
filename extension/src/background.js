import { adForRequest, sendAds } from "utils.js";

chrome.runtime.onMessage.addListener((ads) => {
  let saved = new Set();
  try {
    JSON.parse(localStorage.get("saved")).map((it) => saved.add(it));
  } catch(e) {
    console.log(e);
  }
  const saving = ads.filter((ad) => saved.has(ad.id));
  const success = () => {
    const saved = saving.map((ad) => ad.id);
    localStorage.setItem('saved', JSON.stringify(saved));
  };
  const failure = (e) => console.log(e);
  if(saving.length > 0) sendAds(saving.map(adForRequest)).then(success, failure);
});
