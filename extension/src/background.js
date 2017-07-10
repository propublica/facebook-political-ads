chrome.runtime.onMessage.addListener(() => {
  chrome.browserAction.setBadgeBackgroundColor({
    color: "#1976d2"
  });
});
