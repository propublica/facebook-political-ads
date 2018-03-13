import React from "react";
import Targeting from "./targeting.jsx";
import { Link } from "react-router-dom";
const Ad = ({ ad }) =>
  ad ? (
    <div className="ad">
      <div className="message">
        <div dangerouslySetInnerHTML={{ __html: ad.html }} />
      </div>
      <div className="ad-metadata">
        <Link className="permalink" to={`/facebook-ads/ad/${ad.id}`}>
          Permalink to this ad
        </Link>
        <p>
          First seen:{" "}
          <time dateTime="{ad.created_at}">
            {new Date(Date.parse(ad.created_at)).toLocaleDateString("en-US", {
              day: "numeric",
              month: "long",
              year: "numeric"
            })}
          </time>
        </p>
      </div>
      {ad.targeting !== null ? <Targeting targeting={ad.targeting} /> : ""}
    </div>
  ) : null;

export default Ad;
