import React from "react";
import { connect } from "react-redux";
import { withRouter, Link } from "react-router-dom";
import {
  getStatesAndDistricts,
  newSearch,
  setLang,
  clearAllFilters
} from "actions.js";
import i18next from "i18next";

export class StatesDistricts extends React.Component {
  componentWillMount() {
    const params = new URLSearchParams(location.search);
    this.props.onLoad(); // clears any search stuff that's in state or in the URL.
    this.props.setLang(
      this.props.lang || params.get("lang") || i18next.language
    );
    this.props.getStatesAndDistricts();
  }

  render() {
    return (
      <div id="statesAndDistricts">
        <h1>State</h1>
        <div id="states" className="breakdown">
          {this.props.statesAndDistricts.states ? (
            this.props.statesAndDistricts.states.map(state => (
              <div key={state.abbrev} className="state">
                <Link to={`/facebook-ads/admin/ads?states=${state.abbrev}`}>
                  {state.abbrev}
                </Link>
              </div>
            ))
          ) : (
              <div>
                <h2>Loading...</h2>
              </div>
            )}
        </div>

        <h1>House Districts</h1>
        <div id="districts" className="breakdown">
          {this.props.statesAndDistricts.districts ? (
            Object.entries(this.props.statesAndDistricts.districts)
              .sort((a, b) => (a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0))
              .map(state_districts => (
                <div key={state_districts[0]} className="districts">
                  <h3>{state_districts[0]}</h3>
                  <ul>
                    {state_districts[1]
                      .filter(({ office }) => office === "H")
                      .sort(
                        (a, b) =>
                          a["name"] < b["name"]
                            ? -1
                            : a["name"] > b["name"] ? 1 : 0
                      )
                      .map(district => (
                        <li key={district.id}>
                          <Link
                            to={`/facebook-ads/admin/ads?districts=${
                              district["state"]
                              }-${district["name"]}`}
                          >
                            {district["state"]}-{district["name"]}
                          </Link>
                        </li>
                      ))}
                  </ul>
                </div>
              ))
          ) : (
              <div>
                <h2>Loading...</h2>
              </div>
            )}
        </div>
      </div>
    );
  }
}

const StatesAndDistricts = withRouter(
  connect(
    ({ statesAndDistricts, lang }) => ({
      statesAndDistricts: statesAndDistricts || {},
      lang
    }),
    dispatch => ({
      onLoad: () => dispatch(clearAllFilters()),
      onClick: term => dispatch(newSearch(term)),
      getStatesAndDistricts: (kind, recent) =>
        dispatch(getStatesAndDistricts(kind, recent)),
      setLang: lang => dispatch(setLang(lang))
    })
  )(StatesDistricts)
);
export default StatesAndDistricts;
