import React from "react";
import { Filters } from "components/filters.jsx";
import Pagination from "components/pagination.jsx";
import Term from "components/term.jsx";
import Ad from "components/ad.jsx";
import { t } from "i18n.js";
import { connect } from "react-redux";
import { newSearch } from "actions.js";
import { debounce } from "lodash";
import { withRouter } from "react-router-dom";

const AdListUnconnected = ({ ads, onKeyUp, search }) => (
  <div>
    <form id="facebook-pac-browser" onSubmit={e => e.preventDefault()}>
      <fieldset className="prefabs">
        <legend>{t("search_terms")}</legend>
        <ul>
          {["Trump", "Obama", "Hillary", "Mueller", "Health", "Taxes"].map(
            term => <Term key={term} search={search} term={term} />
          )}
        </ul>
      </fieldset>
      <input
        type="search"
        id="search"
        placeholder={t("search")}
        onChange={onKeyUp}
      />
      <Filters />
    </form>
    <div className="facebook-pac-ads">
      {ads.length > 0 ? (
        <Pagination />
      ) : (
        <p className="no_ads">No ads found for {search}.</p>
      )}
      <div id="ads">{ads.map(ad => <Ad ad={ad} key={ad.id} />)}</div>
      {ads.length > 0 ? <Pagination /> : ""}
    </div>
  </div>
);

const throttledDispatch = debounce((dispatch, input) => {
  dispatch(newSearch(input));
}, 750);

const AdList = withRouter(
  connect(
    ({ ads, search }) => ({ ads, search }),
    dispatch => ({
      onKeyUp: e => {
        e.preventDefault();
        throttledDispatch(
          dispatch,
          e.target.value.length ? e.target.value : null
        );
      }
    })
  )(AdListUnconnected)
);
export default AdList;
