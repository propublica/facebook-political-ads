import React from "react";
import { connect } from "react-redux";
import { withRouter } from "react-router-dom";
import Pagination from "components/pagination.jsx";
import Ad from "components/admin/ad.jsx";
import {
  throttledDispatch,
  getAds,
  changePoliticalProbability,
  throttledDispatchAny,
  clearAdvertisersTargetsAndEntities
} from "actions.js";
import { deserialize } from "utils.js";
import Range from "rc-slider/lib/Range";

export class AdsUnconnected extends React.Component {
  componentDidMount() {
    this.props.deserialize(); // gets params from the URL, dispatches actions.
  }

  render() {
    return (
      <div id="ads">
        <input
          id="search"
          placeholder="Search for ads"
          onKeyUp={this.props.onKeyUp}
          search={this.props.search}
        />
        <div className="rangeslider">
          <label htmlFor="range-1a">Political Likelihood:</label>
          <Range
            defaultValue={[70, 100]}
            marks={{
              10: "10%",
              20: "20%",
              30: "30%",
              40: "40%",
              50: "50%",
              60: "60%",
              70: "70%",
              80: "80%",
              90: "90%"
            }}
            onChange={this.props.onSliderChange}
          />
        </div>

        {this.props.pagination ? <Pagination /> : ""}
        {this.props.ads.length > 0 ? (
          this.props.ads.map(ad => (
            <Ad
              ad={ad}
              key={ad.id}
              onSuppressClick={this.props.onSuppressClick}
            />
          ))
        ) : (
          <div>No ads found (or they&apos;re still loading).</div>
        )}
        {this.props.pagination ? <Pagination /> : ""}
      </div>
    );
  }
}

export const AdsUnrouted = connect(
  ({ ads, search, page, pagination }) => ({
    ads: ads.filter(ad => !ad.suppressed),
    search,
    pagination,
    page
  }),
  dispatch => ({
    deserialize: () => {
      deserialize(dispatch);
      dispatch(getAds());
    },
    onKeyUp: e => {
      e.preventDefault();
      dispatch(clearAdvertisersTargetsAndEntities());
      throttledDispatch(
        dispatch,
        e.target.value.length ? e.target.value : null
      );
    },
    onSliderChange: vals => {
      throttledDispatchAny(dispatch, changePoliticalProbability, vals);
    }
  })
)(AdsUnconnected);
const Ads = withRouter(AdsUnrouted);
export default Ads;
