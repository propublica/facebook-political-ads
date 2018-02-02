import React from "react";
import { connect } from "react-redux";
import { withRouter } from "react-router-dom";
import Pagination from "components/pagination.jsx";
import Ad from "components/admin/ad.jsx";
import { debounce } from "lodash";
import { newSearch } from "actions.js";
import { deserialize, refresh } from "utils.js";
import PropTypes from "prop-types";

class Ads extends React.Component {
  constructor(props, context) {
    // this context stuff is bad and should be factored out once refresh is refactored.
    super(props, context);
  }

  componentDidMount() {
    this.props.deserialize();
    refresh(this.context.store).then(
      () =>
        (this.unsubscribe = this.context.store.subscribe(() =>
          refresh(this.context.store)
        ))
    ); // anytime anything changes, then make the ajax request whenever the user changes the facets they want.
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
        {this.props.ads.map(ad => (
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

// this conAds.contextTypestext stuff is bad and should be factored out once refresh is refactored.
Ads.contextTypes = { store: PropTypes.object }; // temporary, hopefully

Ads = withRouter(
  connect(
    ({ ads, search, page, pagination }) => ({
      ads: ads.filter(ad => !ad.suppressed),
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
      }
    })
  )(Ads)
);
export default Ads;
