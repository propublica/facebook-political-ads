import parser from 'parser';
import throttle from 'lodash/throttle';
import {mergeAds} from 'utils';


let ads = [];
try {
  ads = JSON.parse(localStorage.getItem('parsedAds')) || ads;
} catch(_) {
}

const saveAds = function(ads){
  localStorage.setItem('parsedAds', JSON.stringify(ads));
  return ads;
};

const sendAds = function(){
  let posts = Array.from(document.querySelectorAll(".fbUserContent"))
    .concat(Array.from(document.querySelectorAll(".ego_unit")));
  let sponsor = chrome.i18n.getMessage("sponsor_text");
  let newAds = posts.map((i) =>  parser(i, sponsor)).filter((i) => i);
  console.log(ads, newAds);
  ads = saveAds(mergeAds(ads, newAds));
  chrome.runtime.sendMessage(ads);
};
chrome.runtime.onMessage.addListener((newAds) => ads = saveAds(mergeAds(ads, newAds)));

let observer = new MutationObserver(throttle(sendAds, 250));
observer.observe(document.body, {childList: true, subtree:true});

if(document.readyState === 'complete')
  sendAds();

document.addEventListener('interactive', sendAds, false);
