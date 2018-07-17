import React from "react";
import { connect } from "react-redux";
import PoliticalAdsChart from "components/political_ads_chart.jsx";

const FbpacFeltronsUnconnected = ({ homepage_stats }) => (
  <div className="feltrons">
    <div className="top-feltrons-container">
      <div className="top-feltrons political-ratio">
        <h3>Today</h3>
        <p><strong>
          {homepage_stats && homepage_stats.political_ads_total
            ? homepage_stats.political_ads_total
                .toString()
                .replace(/\B(?=(\d{3})+(?!\d))/g, ",")
            : "--"}
        </strong></p>
        <p>political ads collected</p>
      </div>
      <div className="top-feltrons top-target">
        <h3>Top Targeting Methods</h3>
        <p>location, political affiliation, custom&nbsp;audience</p>
      </div>
      <div className="top-feltrons ads-collected">
        <h3>All Political Ads</h3>
        <p><strong>
          {homepage_stats && homepage_stats.political_ads_total
            ? homepage_stats.political_ads_total
                .toString()
                .replace(/\B(?=(\d{3})+(?!\d))/g, ",")
            : "--"}
        </strong>{" "} ads from</p>
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
);

const FbpacFeltrons = connect(
  ({ homepage_stats }) => ({ homepage_stats }),
  () => ({})
)(FbpacFeltronsUnconnected);
export default FbpacFeltrons;
