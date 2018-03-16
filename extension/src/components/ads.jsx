import React from "react";
import { withI18n } from "i18n.js";
import { RatingType } from "constants.js";
import { rateAd, updateAd, updateRating } from "actions.js";
import { getUnratedRatings } from "utils.js";
import { connect } from "react-redux";

export const Ad = ({ id, html }) => (
  <div className="ad" id={id}>
    <div className="ad-display" dangerouslySetInnerHTML={{ __html: html }} />
  </div>
);

export const RatingForm = withI18n(
  ({ getMessage, rating, action, question }) => (
    <div className="rater">
      {getMessage(question)}
      <button
        id={"political" + rating.id}
        onClick={function() {
          return action(rating, RatingType.POLITICAL);
        }}
      >
        {getMessage("political")}
      </button>
      <button
        id={"normal" + rating.id}
        onClick={function() {
          return action(rating, RatingType.NORMAL);
        }}
      >
        {getMessage("normal")}
      </button>
    </div>
  )
);

// Ads to be rated and sent to the server
export const Rating = withI18n(({ getMessage, rating, action, question }) => (
  <div className="rating">
    {"rating" in rating ? (
      <b className="political">{getMessage("political")}</b>
    ) : (
      <RatingForm action={action} rating={rating} question={question} />
    )}
    <Ad id={rating.id} html={rating.html} />
  </div>
));

export const Ratings = ({ onRatingClick, ratings }) => (
  <div id="ratings">
    {ratings.map(rating => (
      <Rating
        key={rating.id}
        rating={rating}
        action={onRatingClick}
        question="rating_question"
      />
    ))}
  </div>
);
const ratingsStateToProps = state => ({
  ratings: getUnratedRatings(state.ratings)
});
const ratingsDispatchToProps = dispatch => ({
  onRatingClick: (id, rating) => {
    dispatch(rateAd(id, rating, updateRating));
  }
});
export const UnratedRatings = connect(
  ratingsStateToProps,
  ratingsDispatchToProps
)(Ratings);

// Ads from the server to show
export const AdsUnconnected = ({ ads, onAdClick }) => (
  <div id="ads">
    {ads.map(ad => (
      <Rating
        key={ad.id}
        rating={ad}
        action={onAdClick}
        question="verify_question"
      />
    ))}
  </div>
);
export const adStateToProps = state => ({
  ads: getUnratedRatings(state.ads)
});
export const adDispatchToProps = dispatch => ({
  onAdClick: (id, rating) => {
    dispatch(rateAd(id, rating, updateAd));
  }
});
export const Ads = connect(adStateToProps, adDispatchToProps)(AdsUnconnected);
