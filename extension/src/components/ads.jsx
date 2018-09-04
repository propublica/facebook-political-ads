import React from "react";
import { withI18n } from "i18n.js";
import { RatingType } from "constants.js";
import { rateAd, updateAd, updateRating } from "actions.js";
import { getUnratedRatings } from "utils.js";
import { connect } from "react-redux";
import DOMPurify from "dompurify";

export const Ad = ({ id, html }) => (
  <div className="ad" id={id}>
    <div
      className="ad-display"
      dangerouslySetInnerHTML={{ __html: DOMPurify.sanitize(html) }}
    />
  </div>
);

export const RatingForm = withI18n(
  ({ getMessage, rating, action, question }) => (
    <div className="rater">
      {getMessage(question)}
      <button
        id={"political" + rating.id}
        onClick={() => action(rating, RatingType.POLITICAL)}
      >
        {getMessage("political")}
      </button>
      <button
        id={"normal" + rating.id}
        onClick={() => action(rating, RatingType.NORMAL)}
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

export class Ratings extends React.Component {
  componentDidMount() {
    Array.from(document.querySelectorAll(".clearfix._42ef ._5u5j span span"))
      .concat(
        Array.from(document.querySelectorAll(".clearfix._42ef ._5u5j span div"))
      )
      .filter(t => t.textContent.length == 1)
      .forEach(t => t.remove());
  }

  render() {
    return (
      <div id="ratings">
        {this.props.ratings.map(rating => (
          <Rating
            key={rating.id}
            rating={rating}
            action={this.props.onRatingClick}
            question="rating_question"
          />
        ))}
      </div>
    );
  }
}

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
export class AdsUnconnected extends React.Component {
  componentDidMount() {
    Array.from(document.querySelectorAll(".clearfix._42ef ._5u5j span span"))
      .concat(
        Array.from(document.querySelectorAll(".clearfix._42ef ._5u5j span div"))
      )
      .filter(t => t.textContent.length == 1)
      .forEach(t => t.remove());
  }

  render() {
    return (
      <div id="ads">
        {this.props.ads.map(ad => (
          <Rating
            key={ad.id}
            rating={ad}
            action={this.props.onAdClick}
            question="verify_question"
          />
        ))}
      </div>
    );
  }
}
const adStateToProps = state => ({
  ads: getUnratedRatings(state.ads)
});
const adDispatchToProps = dispatch => ({
  onAdClick: (id, rating) => {
    dispatch(rateAd(id, rating, updateAd));
  }
});
export const Ads = connect(adStateToProps, adDispatchToProps)(AdsUnconnected);
