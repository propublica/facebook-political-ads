import React from "react";
import Pagination from "components/pagination.jsx";
import FbpacFeltrons from "components/fbpac_feltrons.jsx";
import SelectorsAndPersonae from "components/selectors_and_personae.jsx";
import PleaseInstall from "components/please_install.jsx";
import KeywordSearch from "components/keyword_search.jsx";
import Ad from "components/ad.jsx";
import { connect } from "react-redux";
import {
  getAds,
  getHomepageSummary,
  hideOldSearch,
  showOldSearch
} from "actions.js";
import { withRouter } from "react-router-dom";
import { deserialize } from "utils.js";

export class AdListUnconnected extends React.Component {
  componentDidMount() {
    // gets params from the URL and dispatches the relevant actions, which'll cause a DidUpdate, and
    // then a getAds
    this.props.deserialize();
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
    return (
      <div>
        <FbpacFeltrons />

        <SelectorsAndPersonae />

        <KeywordSearch />

        <div className="facebook-pac-ads">
          <p className="why-these-ads">
            {this.props.persona
              ? "The following ads target one or more of the traits selected above."
              : "The following ads are the most recent ads submitted to our database. Select traits above to filter based on how they were targeted."}{" "}
            <button
              id="toggle-topic-search"
              onClick={() =>
                this.props.show_old_search
                  ? this.props.hideOldSearch()
                  : this.props.showOldSearch()
              }
            >
              {this.props.show_old_search
                ? "Hide Keyword Search"
                : "Search by Keyword"}
            </button>
          </p>
          {this.props.ads.length > 0 ? (
            <Pagination />
          ) : (
            <p className="no_ads">
              {this.props.search && this.props.search.length > 0
                ? `No ads found for ${this.props.search}`
                : "No ads found"}.
            </p>
          )}

          <div id="ads">
            {this.props.ads.slice(0, 2).map(ad => <Ad ad={ad} key={ad.id} />)}
            {this.props.ads.length > 0 && this.props.persona ? (
              <PleaseInstall />
            ) : null}
            {this.props.ads.slice(2).map(ad => <Ad ad={ad} key={ad.id} />)}
          </div>
          {this.props.ads.length > 0 ? <Pagination /> : ""}
        </div>
      </div>
    );
  }
}

export const AdListUnrouted = connect(
  ({
    ads,
    search,
    pagination,
    filters,
    entities,
    advertisers,
    targets,
    show_old_search,
    persona
  }) => ({
    ads,
    search,
    pagination,
    filters,
    entities,
    advertisers,
    targets,
    show_old_search,
    persona
  }),
  dispatch => ({
    deserialize: () => {
      deserialize(dispatch, ["en-US", "de-DE"]);
      dispatch(getAds());
      dispatch(getHomepageSummary());
    },
    hideOldSearch: () => {
      window.location.hash = "";
      dispatch(hideOldSearch());
    },
    showOldSearch: () => {
      window.location.hash = "facebook-pac-browser";
      dispatch(showOldSearch());
    }
  })
)(AdListUnconnected);
const AdList = withRouter(AdListUnrouted);
export default AdList;
