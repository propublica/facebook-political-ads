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

export class StatesAndDistrictsUnrouted extends React.Component {
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
        <p>
          You can see the list of candidates we know about{" "}
          <a href="/fbpac-api/candidates">here</a>; you can edit them, delete or{" "}
          add add new ones there too. Email Jeremy if you want to add a bunch.{" "}
        </p>
        <h1>Party</h1>
        <div id="parties" className="breakdown">
          <div key="dem" className="state">
            <Link to="/facebook-ads/admin/ads?parties=DEM,DFL">
              Democratic candidate ads
            </Link>
          </div>
          <div key="gop" className="state">
            <Link to="/facebook-ads/admin/ads?parties=REP,GOP">
              Republican candidate ads
            </Link>
          </div>
        </div>

        <h1>Ads by State and Race</h1>

        <table id="states">
          <thead>
            <tr>
              <th>State</th>
              <th>Ads Targeting A State Or Mentioning a Candidate</th>
              <th>Ads Bought by Candidates</th>
              <th>Ads in Specific Races</th>
            </tr>
          </thead>
          <tbody>
            {this.props.statesAndDistricts.states ? (
              this.props.statesAndDistricts.states.map(state => (
                <tr key={state.abbrev} className="state">
                  <th>{state.name}</th>
                  <td>
                    <Link to={`/facebook-ads/admin/by_state/${state.abbrev}`}>
                      {state.abbrev}
                    </Link>
                  </td>
                  <td>
                    <Link to={`/facebook-ads/admin/ads?states=${state.abbrev}`}>
                      {state.abbrev}
                    </Link>
                  </td>
                  <td>
                    {this.props.statesAndDistricts.districts &&
                    this.props.statesAndDistricts.districts[state.abbrev] ? (
                      this.props.statesAndDistricts.districts[state.abbrev]
                        .concat(
                          this.props.statesAndDistricts.state_races[
                            state.abbrev
                          ] || []
                        )
                          .sort(
                          (a, b) =>
                            a["name"] < b["name"]
                              ? -1
                              : a["name"] > b["name"] ? 1 : 0
                        )
                        .map(district => (
                            <span key={district.id}>
                            {" "}
                            <Link
                                to={`/facebook-ads/admin/ads?districts=${
                                district["state"]
                              }-${district["name"]}`}
                            >
                              {district.office == "H"
                                ? `${district["state"]}-${district["name"] ||
                                    "at large"}`
                                : district["name"]}
                            </Link>{" "}
                            </span>
                        ))
                    ) : (
                        <span>(none)</span>
                    )}
                  </td>
                </tr>
              ))
            ) : (
              <tr>
                <td>Loading...</td>
              </tr>
            )}
          </tbody>
        </table>
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
  )(StatesAndDistrictsUnrouted)
);
export default StatesAndDistricts;
