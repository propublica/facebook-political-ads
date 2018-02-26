import React from "react";
import { Filters } from "components/filters.jsx";
import Pagination from "components/pagination.jsx";
import Term from "components/term.jsx";
import Ad from "components/ad.jsx";
import { t } from "i18n.js";
import { connect } from "react-redux";
import { throttledDispatch, getAds } from "actions.js";
import { withRouter } from "react-router-dom";
import { deserialize } from "utils.js";

export class AdListUnconnected extends React.Component {
  componentDidMount() {
    this.props.deserialize(); // gets params from the URL and dispatches the relevant actions, which'll cause a DidUpdate, and then a getAds
  }

  render() {
    return (
      <div>
        <form id="facebook-pac-browser" onSubmit={e => e.preventDefault()}>
          <fieldset className="prefabs">
            <legend>{t("search_terms")}</legend>
            <ul>
              {["Trump", "Obama", "Hillary", "Mueller", "Health", "Taxes"].map(
                term => (
                  <Term key={term} search={this.props.search} term={term} />
                )
              )}
            </ul>
          </fieldset>
          <input
            type="search"
            id="search"
            placeholder={t("search")}
            onChange={this.props.onChange}
          />
          <Filters />
        </form>
        <div className="facebook-pac-ads">
          {this.props.ads.length > 0 ? (
            <Pagination />
          ) : (
            <p className="no_ads">No ads found for {this.props.search}.</p>
          )}
          <div id="ads">
            {this.props.ads.map(ad => <Ad ad={ad} key={ad.id} />)}
          </div>
          {this.props.ads.length > 0 ? <Pagination /> : ""}
        </div>
      </div>
    );
  }
}

export const AdListUnrouted = connect(
  ({ ads, search, pagination, filters, entities, advertisers, targets }) => ({
    ads,
    search,
    pagination,
    filters,
    entities,
    advertisers,
    targets
  }),
  dispatch => ({
    onChange: e => {
      e.preventDefault();
      throttledDispatch(
        dispatch,
        e.target.value.length ? e.target.value : null
      );
    },
    deserialize: () => {
      deserialize(dispatch);
      dispatch(getAds());
    }
  })
)(AdListUnconnected);
const AdList = withRouter(AdListUnrouted);
export default AdList;
