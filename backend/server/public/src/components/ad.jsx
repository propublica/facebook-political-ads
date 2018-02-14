import React from "react";
import Targeting from "./targeting.jsx";

const Ad = ({ ad }) => (
  <div className="ad">
    <div className="message">
      <div dangerouslySetInnerHTML={{ __html: ad.html }} />
    </div>
    {ad.targeting !== null ? <Targeting targeting={ad.targeting} /> : ""}
  </div>
);
export default Ad;
