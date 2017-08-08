const adForRequest = (ad) => ({
  id: ad.id,
  html: ad.html,
  browser_lang: chrome.i18n.getUILanguage()
});


export { adForRequest };
