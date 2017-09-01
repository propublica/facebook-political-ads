const adForRequest = (ad) => (ad);

const endpoint = process.env.NODE_ENV === 'production' ?
  "https://projects.propublica.org/facebook-ads/ads" :
  "http://0.0.0.0:8080/facebook-ads/ads";
const sendAds = (body) => fetch(endpoint, {
  method: "POST",
  mode: 'no-cors',
  body: JSON.stringify(body)
});

const getAds = (cb) => fetch(endpoint).then((res) => res.json()).then(cb);

export { adForRequest, sendAds };
