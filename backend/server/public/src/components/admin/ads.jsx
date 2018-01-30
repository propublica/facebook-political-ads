import React from "react";
import { connect } from "react-redux";
import Pagination from "components/pagination.jsx";
import Ad from "components/admin/ad.jsx";
import { debounce } from "lodash";
import { newSearch } from "actions.js";

let Ads = ({ ads, onKeyUp, search, pagination }) => (
  <div id="ads">
    <input
      id="search"
      placeholder="Search for ads"
      onKeyUp={onKeyUp}
      search={search}
    />
    {pagination ? <Pagination /> : ""}
    {ads.map(ad => <Ad ad={ad} key={ad.id} />)}
  </div>
);
const throttledDispatch = debounce((dispatch, input) => {
  dispatch(newSearch(input));
}, 750);
Ads = connect(
  ({ ads, search, pagination }) => ({
    ads: ads.filter(ad => !ad.suppressed),
    search,
    pagination
  }),
  dispatch => ({
    onKeyUp: e => {
      e.preventDefault();
      throttledDispatch(
        dispatch,
        e.target.value.length ? e.target.value : null
      );
    }
  })
)(Ads);
export default Ads;
