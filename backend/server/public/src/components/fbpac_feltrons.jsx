import React from "react";
import { connect } from "react-redux";
import PoliticalAdsChart from "components/political_ads_chart.jsx";

const FbpacFeltronsUnconnected = ({ homepage_stats }) => (
  <div className="feltrons">
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
        <p>location &#8226; politics &#8226; custom audience &#8226; age &#8226; look-a-like audience</p>
      </div>
      <div className="top-feltrons ads-collected">
        <h3>Total Political Ads</h3>
        <p><strong>
          {homepage_stats && homepage_stats.user_count
            ? homepage_stats.user_count
                .toString()
                .replace(/\B(?=(\d{3})+(?!\d))/g, ",")
            : "--"}
        </strong>{" "}contributors collected</p>
        <p><strong>
          {homepage_stats && homepage_stats.political_ads_total
            ? homepage_stats.political_ads_total
                .toString()
                .replace(/\B(?=(\d{3})+(?!\d))/g, ",")
            : "--"}
        </strong>{" "} political ads</p>
      </div>
    </div>
  </div>
);

const FbpacFeltrons = connect(
  ({ homepage_stats }) => ({ homepage_stats }),
  () => ({})
)(FbpacFeltronsUnconnected);
export default FbpacFeltrons;
