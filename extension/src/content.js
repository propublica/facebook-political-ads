import parser from 'parser';
import throttle from 'lodash/throttle';

const sendAds = function(){
  let posts = Array.from(document.querySelectorAll(".fbUserContent, .fbUserPost, ._5pcr"))
    .concat(Array.from(document.querySelectorAll(".ego_unit")));
  let ads = posts.map((i) => parser(i)).filter((i) => i);
  chrome.runtime.sendMessage(ads);
};

const throttled = throttle(sendAds, 250);

let observer = new MutationObserver(throttled);
observer.observe(document.body, {childList: true, subtree:true});
setInterval(throttled, 500);

if(document.readyState === 'complete')
  sendAds();
document.addEventListener('interactive', sendAds, false);
