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
  clearAllFilters
} from "actions.js";
import { deserialize } from "utils.js";
import Range from "rc-slider/lib/Range";

export class AdsUnconnected extends React.Component {
  componentDidMount() {
    this.props.deserialize(); // gets params from the URL, dispatches actions.
  }
  componentDidUpdate() {
    Array.from(document.querySelectorAll(".clearfix._42ef ._5u5j span div"))
      .filter(t => t.textContent.length == 1)
      .forEach(t => t.remove());
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
        <div className="filters">
          Active filters:{" "}
          {this.props.states.length > 0 ? (
            <span>
              States: <strong>{this.props.states.join(", ")} </strong>
            </span>
          ) : null}
          {this.props.districts.length > 0 ? (
            <span>
              Districts: <strong>{this.props.districts.join(", ")} </strong>
            </span>
          ) : null}
          {this.props.parties.length > 0 ? (
            <span>
              Parties: <strong>{this.props.parties.join(", ")} </strong>
            </span>
          ) : null}
          {this.props.advertisers.length > 0 ? (
            <span>
              Advertisers:{" "}
              <strong>
                {this.props.advertisers
                  .map(filter => filter.advertiser)
                  .join(", ")}{" "}
              </strong>
            </span>
          ) : null}
          {this.props.targets.length > 0 ? (
            <span>
              Targets:{" "}
              <strong>
                {this.props.targets
                  .map(
                    filter =>
                      filter.segment
                        ? `${filter.target} → ${filter.segment}`
                        : filter.target
                  )
                  .join(", ")}{" "}
              </strong>
            </span>
          ) : null}
          {this.props.entities.length > 0 ? (
            <span>
              Entities:{" "}
              <strong>
                {this.props.entities.map(filter => filter.entity).join(", ")}{" "}
              </strong>
            </span>
          ) : null}
          <a href="#" onClick={this.props.clearAllFilters}>
            Clear All
          </a>
        </div>
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
  ({
    ads,
    search,
    page,
    pagination,
    states,
    districts,
    parties,
    targets,
    entities,
    advertisers
  }) => ({
    ads: ads.filter(ad => !ad.suppressed),
    search,
    pagination,
    page,

    states,
    districts,
    parties,
    targets: targets.filter(it => it.active),
    entities: entities.filter(it => it.active),
    advertisers: advertisers.filter(it => it.active)
  }),
  dispatch => ({
    deserialize: () => {
      deserialize(dispatch);
      dispatch(getAds());
    },
    onKeyUp: e => {
      e.preventDefault();
      dispatch(clearAllFilters());
      throttledDispatch(
        dispatch,
        e.target.value.length ? e.target.value : null
      );
    },
    onSliderChange: vals => {
      throttledDispatchAny(dispatch, changePoliticalProbability, vals);
    },
    clearAllFilters: () => {
      dispatch(clearAllFilters());
      dispatch(getAds());
    }
  })
)(AdsUnconnected);
const Ads = withRouter(AdsUnrouted);
export default Ads;
