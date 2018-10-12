import React from "react";
import { connect } from "react-redux";
import Ad from "components/ad.jsx";
import { withRouter, Link } from "react-router-dom";
import { getOneAd } from "../actions.js";
import PropTypes from "prop-types"; // ES6

export class AdDetailUnconnected extends React.Component {
  componentDidMount() {
    const ad_id = this.props.match.params.ad_id;
    // `match` is from React Router -- it's the bit of the URL that matches.
    this.props.getOneAd(ad_id);
  }
  componentDidUpdate() {
    Array.from(document.querySelectorAll(".clearfix._42ef ._5u5j span span"))
      .concat(
        Array.from(document.querySelectorAll(".clearfix._42ef ._5u5j span div"))
      )
      .filter(t => t.textContent.length == 1)
      .forEach(t => t.remove());
  }

  render() {
    let ad_or_error = null;

    if (this.props.ad) {
      if (this.props.ad.id) {
        ad_or_error = (
          <div id="ad">
            <Ad ad={this.props.ad} />
          </div>
        );
      } else {
        ad_or_error = (
          <h2 className="notfound">
            Uh oh, an ad with that ID couldn&apos;t be found!
          </h2>
        );
      }
    } else {
      ad_or_error = <h2 className="error">Uh oh, something went wrong.</h2>;
    }
    return (
      <div>
        <Link className="all-link prefab" to="/facebook-ads">
          Return to list of all ads
        </Link>
        <div className="facebook-pac-ads">{ad_or_error}</div>
      </div>
    );
  }
}
AdDetailUnconnected.propTypes = {
  match: PropTypes.object.isRequired,
  ad: PropTypes.object
};

const AdDetail = withRouter(
  connect(
    ({ ad }) => ({
      // this is a mapStateToProps function. { ad } is destructuring the `store` hash and getting the `ads` element.
      ad
    }),
    dispatch => ({
      // ownProps is available as a second argument here.
      getOneAd: ad => dispatch(getOneAd(ad))
    })
  )(AdDetailUnconnected)
);

export default AdDetail;
