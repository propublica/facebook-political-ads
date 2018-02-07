import React from "react";
import Targeting from "./targeting.jsx";
import { getOneAd } from "../actions.js";
import { connect } from "react-redux";

class Ad extends React.Component {
  componentDidMount() {
    // if this.props.ad, we're good, we got the ad prop from <Frontend> or it's already been fetched.
    // if !this.props.ad && !this.props.getOneAd, then something has gone wrong.
    if (!this.props.ad.id && this.props.getOneAd && this.props.match) {
      let ad_id = null;
      // `match` is from React Router -- it's the bit of the URL that matches.
      ad_id = this.props.match.params.ad_id;
      console.log("match", ad_id);
      this.props.getOneAd(ad_id);
    }
  }

  render() {
    return this.props.ad ? (
      <div>
        <div className="message">
          <div dangerouslySetInnerHTML={{ __html: this.props.ad.html }} />
        </div>
        {this.props.ad.targeting !== null ? (
          <Targeting targeting={this.props.ad.targeting} />
        ) : (
          ""
        )}
      </div>
    ) : null;
  }
}

export const AdRedux = connect(
  ({ ad }) => ({ ad }),
  dispatch => ({ getOneAd: ad => dispatch(getOneAd(ad)) })
)(Ad);

export default Ad;
