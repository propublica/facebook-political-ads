import React from "react";
import { connect } from "react-redux";
import PoliticalAdsChart from "components/political_ads_chart.jsx";

const FbpacFeltronsUnconnected = ({ homepage_stats }) => (
  <div className="feltrons">
    <div className="feltron-static" />
    <div className="top-feltrons-container">
      <div className="top-feltrons political-ratio">
        <h3>This Week's Political Ads</h3>
        <PoliticalAdsChart
          political_ads_per_day={
            homepage_stats && homepage_stats.political_ads_per_day
              ? homepage_stats.political_ads_per_day.slice(
                0,
                homepage_stats.political_ads_per_day.length - 1
              )
              : null
          }
        />
      </div>
      <div className="top-feltrons top-target">
        <h3>Top targeted methods</h3>
        <div className="flexy">
          <div className="">
            <p>location, age, custom audience,</p>
            <p>politics, look-a-like audience</p>
          </div>
        </div>
      </div>
      <div className="top-feltrons ads-collected">
        <div className="flexy">
          <div className="">
            <p><strong>
              {homepage_stats && homepage_stats.political_ads_total
                ? homepage_stats.political_ads_total
                    .toString()
                    .replace(/\B(?=(\d{3})+(?!\d))/g, ",")
                : "--"}
            </strong>{" "} political ads collected by</p>
            <p><strong>
              {homepage_stats && homepage_stats.user_count
                ? homepage_stats.user_count
                    .toString()
                    .replace(/\B(?=(\d{3})+(?!\d))/g, ",")
                : "--"}
            </strong>{" "}contributors</p>
          </div>
        </div>
      </div>
    </div>
  </div>
);

const FbpacFeltrons = connect(
  ({ homepage_stats }) => ({ homepage_stats }),
  () => ({})
)(FbpacFeltronsUnconnected);
export default FbpacFeltrons;
