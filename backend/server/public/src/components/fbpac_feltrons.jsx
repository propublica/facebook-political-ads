import React from "react";
import { connect } from "react-redux";
import PoliticalRatioChart from "components/political_ratio_chart.jsx";

const FbpacFeltronsUnconnected = ({ homepage_stats }) => (
  <div className="feltrons">
  <div className="feltron-static"></div>
    <div className="top-feltrons-container">
      <div className="top-feltrons political-ratio">
        <h3>More Political Ads Lately?</h3>
        <PoliticalRatioChart
          weekly_political_ratio={
            homepage_stats && homepage_stats.weekly_political_ratio
              ? homepage_stats.weekly_political_ratio.slice(
                  0,
                  homepage_stats.weekly_political_ratio.length - 1
                )
              : null
          }
        />
      </div>
      <div className="top-feltrons top-target">
        <h3>Top targeted traits</h3>
        <div className="flexy">
          <div className="">
            <p>Facebook users similar to the advertiser's current customers</p>
            <p>and</p>
            <p>Users on a particular Facebook list</p>
          </div>
        </div>
      </div>
    </div>
    <div className="stats-sentence">
      <p>
        <strong>{homepage_stats && homepage_stats.user_count
          ? homepage_stats.user_count
            .toString()
            .replace(/\B(?=(\d{3})+(?!\d))/g, ",")
          : "--"}</strong>
         {" "}readers like you have helped collect{" "}
        <strong>{homepage_stats && homepage_stats.political_ads_total
          ? homepage_stats.political_ads_total
            .toString()
            .replace(/\B(?=(\d{3})+(?!\d))/g, ",")
          : "--"}</strong>
        {" "}political ads.
      </p>
    </div>
  </div>
);

const FbpacFeltrons = connect(
  ({ homepage_stats }) => ({ homepage_stats }),
  () => ({})
)(FbpacFeltronsUnconnected);
export default FbpacFeltrons;
