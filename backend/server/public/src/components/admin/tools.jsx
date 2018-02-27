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
        <Link to="/facebook-ads/admin/grouped/by_advertiser">
          All ads grouped by advertiser
        </Link>
      </li>
      <li>
        <Link to="/facebook-ads/admin/grouped/recent_by_advertiser">
          Recent ads grouped by advertiser
        </Link>
      </li>
      {/* <li>
        <Link to="/facebook-ads/admin/grouped/recent_segments">
          All ads grouped by targeting segment
        </Link>{" "}
        (i.e. all ads targeted at people over 18, or all ads targeted by gender
        only to men)
      </li>
      <li>
        <Link to="/facebook-ads/admin/grouped/segments">
          Recent ads grouped by targeting segment
        </Link>{" "}
        (i.e. all ads targeted at people over 18, or all ads targeted by gender
        only to men)
      </li> */}
      <li>
        <Link to="/facebook-ads/admin/ads?search=Donald+J.+Trump&advertisers=%5B%22Donald+J.+Trump%22%5D">
          Trump ads
        </Link>
      </li>
    </ul>
  </div>
);

export default AdminTools;
