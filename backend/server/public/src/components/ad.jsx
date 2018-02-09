import React from "react";
import Targeting from "./targeting.jsx";
import { Link } from "react-router-dom";
const Ad = ({ ad }) =>
  ad ? (
    <div className="ad">
      <div className="message">
        <div dangerouslySetInnerHTML={{ __html: ad.html }} />
      </div>
      <Link className="permalink" to={`/facebook-ads/ad/${ad.id}`}>
        Permalink to this ad
      </Link>
      {ad.targeting !== null ? <Targeting targeting={ad.targeting} /> : ""}
    </div>
  ) : null;

export default Ad;
