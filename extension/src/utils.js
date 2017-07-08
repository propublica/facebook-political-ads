const mergeAds = (ads, newAds) => {
  let ids = new Set(ads.map(ad => ad.id));
  return newAds.reduce((state, ad) => (
    ids.has(ad.id) ? state : state.concat([ad])
  ), ads);
};

const getUnratedRatings = (ratings) => (
  ratings.filter(rating => !("rating" in rating))
);


export {mergeAds, getUnratedRatings};
