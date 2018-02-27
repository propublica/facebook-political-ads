import React from "react";
import { Link } from "react-router-dom";

const AdminTools = () => (
  <div>
    <p>
      Here are some tools to hopefully help you find interesting and newsworthy
      ads.
    </p>
    <ul className="tools">
      <li>
        <Link to="/facebook-ads/admin/grouped/advertisers">
          Ads grouped by advertiser
        </Link>
      </li>
      <li>
        <Link to="/facebook-ads/admin/grouped/recentadvertisers">
          Recent ads grouped by advertiser
        </Link>
      </li>
      <li>
        <Link to="/facebook-ads/admin/ads?search=Donald+J.+Trump&advertisers=%5B%22Donald+J.+Trump%22%5D">
          Trump ads
        </Link>
      </li>
    </ul>
  </div>
);

export default AdminTools;
