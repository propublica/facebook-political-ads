const adForRequest = (ad) => ({
  id: ad.id,
  html: ad.html,
  targeting: ad.targeting
});

const endpoint = process.env.NODE_ENV === 'production' ?
  "https://projects.propublica.org/facebook-ads/ads" :
  "http://0.0.0.0:8080/facebook-ads/ads";

const headers = (language) => new Headers({
  "Accept-Language": language.language + "-" + language.country
});

const sendAds = (body, language) => fetch(endpoint, {
  method: "POST",
  mode: 'no-cors',
  headers: headers(language),
  body: JSON.stringify(body)
});

const getAds = (language, cb) => fetch(endpoint, {
  method: "GET",
  headers: headers(language),
}).then((res) => res.json()).then(cb);

const mergeAds = (ads, newAds) => {
  let ids = new Map(ads.map(ad => [ad.id, ad]));
  newAds.forEach(ad => {
    if(ids.has(ad.id)) {
      let old = ids.get(ad.id);
      ids.delete(ad.id);
      let newAd = Object.assign({}, old, ad);
      ids.set(newAd.id, newAd);
    } else {
      ids.set(ad.id, ad);
    }
  });
  return Array.from(ids.values()).sort((a, b) => a.id > b.id ? 1 : -1);
};

const updateBadge = (ratings) => {
  const num = ratings.filter(rating => !("rating" in rating)).length;
  if(num > 0) {
    chrome.browserAction.setBadgeText({text: num > 100 ? "100+" : "" + num});
  } else {
    chrome.browserAction.setBadgeText({text: ""});
  }
};

const getBrowserLocale = () => ({
  language: navigator.language.split("-")[0],
  country: navigator.language.split("-")[1]
});

export { sendAds, getAds, mergeAds, updateBadge, adForRequest, getBrowserLocale };
