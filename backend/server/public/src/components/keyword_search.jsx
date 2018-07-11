import React from "react";
import { getAds, setPersona } from "actions.js";
import { connect } from "react-redux";
import { t } from "i18n.js";
import Term from "components/term.jsx";
import { Filters } from "components/filters.jsx";
import { throttledDispatch } from "actions.js";

const KeywordSearchUnconnected = ({ show_old_search, search, onChange }) => (
        show_old_search ? (<form id="facebook-pac-browser" onSubmit={e => e.preventDefault()}>
        <fieldset className="prefabs">
          <legend>{t("search_terms")}</legend>
          <ul>
            {["Trump", "Obama", "Hillary", "Mueller", "Health", "Taxes"].map(
              term => (
                <Term key={term} search={search} term={term} />
              )
            )}
          </ul>
        </fieldset>
        <input
          type="search"
          id="search"
          placeholder={t("search")}
          onChange={onChange}
        />
        <Filters />
      </form>
      ) : null
    );

const KeywordSearch = connect(
    ({ search, show_old_search }) => ({
      search,
      show_old_search
    }),
    dispatch => ({
      onChange: e => {
        e.preventDefault();
        throttledDispatch(
          dispatch,
          e.target.value.length ? e.target.value : null
        );
      },
    })
  )(KeywordSearchUnconnected);
export default KeywordSearch;
  