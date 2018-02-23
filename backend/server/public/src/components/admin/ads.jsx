import React from "react";
import { connect } from "react-redux";
import { withRouter } from "react-router-dom";
import Pagination from "components/pagination.jsx";
import Ad from "components/admin/ad.jsx";
import { debounce } from "lodash";
import { newSearch, getAds } from "actions.js";
import { deserialize } from "utils.js";
import PropTypes from "prop-types";

export class AdsUnconnected extends React.Component {
  componentDidMount() {
    this.props.deserialize(); // gets params from the URL, dispatches actions.
  }

  componentDidUpdate({ search, pagination }) {
    if (
      search !== this.props.search ||
      this.props.pagination.page !== pagination.page
    ) {
      this.props.getAds();
    }
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
        {this.props.pagination ? <Pagination /> : ""}
        {this.props.ads
          .filter(ad => !ad.suppressed)
          .map(ad => (
            <Ad
              ad={ad}
              key={ad.id}
              onSuppressClick={this.props.onSuppressClick}
            />
          ))}
      </div>
    );
  }

  componentWillUnmount() {
    if (this.unsubscribe) this.unsubscribe();
  }
}

const throttledDispatch = debounce((dispatch, input) => {
  dispatch(newSearch(input));
}, 750);

// this AdsUnconnected.contextTypes stuff is bad and should be factored out once refresh is refactored.
AdsUnconnected.contextTypes = { store: PropTypes.object }; // temporary, hopefully

export const AdsUnrouted = connect(
  ({ ads, search, page, pagination }) => ({
    ads,
    search,
    pagination,
    page
  }),
  dispatch => ({
    deserialize: () => deserialize(dispatch),
    onKeyUp: e => {
      e.preventDefault();
      throttledDispatch(
        dispatch,
        e.target.value.length ? e.target.value : null
      );
    },
    getAds: () => dispatch(getAds())
  })
)(AdsUnconnected);
const Ads = withRouter(AdsUnrouted);
export default Ads;
