import React from "react";
import { connect } from "react-redux";
import Ad from "components/admin/ad.jsx";
import { getOneAd, suppressAd } from "actions.js";
import { withRouter } from "react-router-dom";

export class AdDetailUnconnected extends React.Component {
  componentDidMount() {
    let ad_id = null;
    if (this.props.match) {
      // `match` is from React Router -- it's the bit of the URL that matches.
      ad_id = this.props.match.params.ad_id;
      this.props.getOneAd(ad_id);
    }
  }

  render() {
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
            <h2 className="notfound">
              Uh oh, an ad with that ID couldn&apos;t be found!
            </h2>
          </div>
        );
      }
    } else {
      return (
        <div>
          <h2 className="error">Uh oh, something went wrong.</h2>
        </div>
      );
    }
  }
}
const AdDetail = withRouter(
  connect(
    ({ ad }) => ({
      // this is a mapStateToProps function. { ad } is destructuring the `store` hash and getting
      // the `ads` element.
      ad
    }),
    dispatch => ({
      // ownProps is available as a second argument here.
      getOneAd: ad => dispatch(getOneAd(ad)),
      onSuppressClick: ad => dispatch(suppressAd(ad))
    })
  )(AdDetailUnconnected)
);

export default AdDetail;
