import React from "react";
import { connect } from "react-redux";

const FbpacFeltronsUnconnected = ({ homepage_stats }) => (
  <div className="feltrons">
    <span>
      <strong>Feltrons go here</strong>: Collected, targeted counts, etc.
    </span>
    <div>
      {homepage_stats ? homepage_stats.political_ads_today : "--"} ads today |{" "}
      {homepage_stats ? homepage_stats.political_ads_total : "--"} ads overall
    </div>
    <div>
      ~{homepage_stats ? homepage_stats.political_ads_ratio * 100 : "--"}% of US
      Facebook ads are political
    </div>
    <div>{homepage_stats ? homepage_stats.user_count : "--"} users.</div>
  </div>
);

const FbpacFeltrons = connect(
  ({ homepage_stats }) => ({ homepage_stats }),
  () => ({})
)(FbpacFeltronsUnconnected);
export default FbpacFeltrons;
