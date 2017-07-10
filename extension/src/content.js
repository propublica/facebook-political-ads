import parser from 'parser';
import throttle from 'lodash/throttle';
import {mergeAds} from 'utils';


let ads = [];

const sendAds = function(){
  let posts = Array.from(document.querySelectorAll(".fbUserContent"))
    .concat(Array.from(document.querySelectorAll(".ego_unit")));
  let sponsor = chrome.i18n.getMessage("sponsor_text");
  let newAds = posts.map((i) =>  parser(i, sponsor)).filter((i) => i);
  ads = mergeAds(newAds, ads);
  chrome.runtime.sendMessage(ads);
};

let observer = new MutationObserver(throttle(sendAds, 250));
observer.observe(document.body, {childList: true, subtree:true});

if(document.readyState === 'complete')
  sendAds();

document.addEventListener('interactive', sendAds, false);
