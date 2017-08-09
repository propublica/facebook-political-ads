const adForRequest = (ad) => ({
  id: ad.id,
  html: ad.html,
  browser_lang: chrome.i18n.getUILanguage()
});

const endpoint = process.env.NODE_ENV === 'production' ?
  "https://projects.propublica.org/facebook-ads/ads" :
  "http://0.0.0.0:8080/facebook-ads/ads";
const sendAds = (body) => fetch(endpoint, {
  method: "POST",
  mode: 'no-cors',
  body: JSON.stringify([body])
});

export { adForRequest, sendAds };
