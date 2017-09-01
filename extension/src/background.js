import { sendAds } from "utils.js";

chrome.runtime.onMessage.addListener((ads) => {
  if(!localStorage.getItem("redux")) return;
  try {
    if(!JSON.parse(localStorage.getItem("redux"))["terms"]) return;
  } catch(e) {
    return;
  }
  let saved = new Set();
  if(localStorage.getItem("saved")) {
    try {
      JSON.parse(localStorage.getItem("saved")).map((it) => saved.add(it));
    } catch(e) {
      console.log(e);
    }
  }
  const saving = ads.filter((ad) => !saved.has(ad.id));
  console.log(saving);
  saving.forEach((ad) => saved.add(ad.id));
  localStorage.setItem('saved', JSON.stringify(Array.from(saved)));
  const success = () => { console.log('saved') };
  const failure = (e) => console.log(e);
  if(saving.length > 0) sendAds(saving).then(success, failure);
});
