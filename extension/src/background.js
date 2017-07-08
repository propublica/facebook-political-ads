chrome.runtime.onMessage.addListener((ads) => {
  chrome.browserAction.setBadgeText({
    text: ads.filter(rating => !("rating" in rating)).length + ''
  });
});
