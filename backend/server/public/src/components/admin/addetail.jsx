import React from "react";
import { connect } from "react-redux";
import Ad from "components/admin/ad.jsx";
import { getOneAd, suppressAd } from "actions.js";
import { withRouter } from "react-router-dom";

class AdDetail extends React.Component {
  componentDidMount() {
    let ad_id = null;
    if (this.props.match) {
      // `match` is from React Router -- it's the bit of the URL that matches.
      ad_id = this.props.match.params.ad_id;
    } else {
      ad_id = this.props.requested_ad_id; // really state.permalinked_ad.requested_ad_id
    }
    this.props.getOneAd(ad_id);
  }

  render() {
    console.log(this.props);
    if (this.props.ad) {
      if (this.props.ad.id) {
        return (
          <div id="ad">
            <input id="search" placeholder="Search for ads" />

            <Ad
              ad={this.props.ad}
              onSuppressClick={this.props.onSuppressClick}
            />
          </div>
        );
      } else {
        return (
          <div>
            <h2>Uh oh, an ad with that ID couldn&apos;t be found!</h2>
          </div>
        );
      }
    } else {
      return (
        <div>
          <h2>Loading...</h2>
        </div>
      );
    }
  }
}
AdDetail = withRouter(
  connect(
    ({ ad }) => ({
      // this is a mapStateToProps function. { ad } is destructuring the `store` hash and getting the `ads` element.
      ad
    }),
    dispatch => ({
      // ownProps is available as a second argument here.
      getOneAd: ad => dispatch(getOneAd(ad)),
      onSuppressClick: ad => dispatch(suppressAd(ad))
    })
  )(AdDetail)
);

export default AdDetail;
