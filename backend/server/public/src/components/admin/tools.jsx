import React from "react";
import { Link } from "react-router-dom";

const AdminTools = () => (
  <div id="tools">
    <p>
      Here are some tools to hopefully help you find interesting and newsworthy
      ads. <Link to="/facebook-ads/admin/summary">Summary Stats</Link>
    </p>

    <div className="tools-section">
      <h3>ads grouped by advertiser</h3>
      <p>(e.g. ads from Bernie Sanders)</p>
      <ul>
        <li>
          <Link to="/facebook-ads/admin/grouped/advertiser">all</Link>
        </li>
        <li>
          <Link to="/facebook-ads/admin/grouped/advertiser/this_month">
            first submitted this month
          </Link>
        </li>
        <li>
          <Link to="/facebook-ads/admin/grouped/advertiser/this_week">
            first submitted this week
          </Link>
        </li>
      </ul>
    </div>

    <div className="tools-section">
      <h3>ads grouped by targeting segment</h3>
      <p>
        (e.g. all ads targeted at people over 18, or all ads targeted by gender
        only to men)
      </p>
      <ul>
        <li>
          <Link to="/facebook-ads/admin/grouped/segment">all</Link>
        </li>
        <li>
          <Link to="/facebook-ads/admin/grouped/segment/this_month">
            submitted this month
          </Link>
        </li>
        <li>
          <Link to="/facebook-ads/admin/grouped/segment/this_week">
            submitted this week
          </Link>
        </li>
      </ul>
    </div>

    <div className="tools-section">
      <h3>ads grouped by targeting method</h3>
      <p> (e.g. all ads targeted by age; or all ads targeted by a Like)</p>
      <ul>
        <li>
          <Link to="/facebook-ads/admin/grouped/target">all</Link>
        </li>
        <li>
          <Link to="/facebook-ads/admin/grouped/target/this_month">
            submitted this month
          </Link>
        </li>
        <li>
          <Link to="/facebook-ads/admin/grouped/target/this_week">
            submitted this week
          </Link>
        </li>
      </ul>
    </div>

    <div className="tools-section">
      <h3>US politics stuff</h3>
      <p>
        Let me know if we're missing someone! This should be all federal races
        plus statewide state races (governor, lt gov, etc.)
      </p>
      <ul>
        <li>
          <Link to="/facebook-ads/admin/ads?search=Donald+J.+Trump&advertisers=%5B%22Donald+J.+Trump%22%5D">
            Trump ads
          </Link>
        </li>

        <li>
          <Link to="/facebook-ads/admin/ads?parties=DEM,DFL">
            Democratic candidate ads
          </Link>
        </li>

        <li>
          <Link to="/facebook-ads/admin/ads?parties=REP,GOP">
            Republican candidate ads
          </Link>
        </li>
        <li>
          <Link to="/facebook-ads/admin/states">
            Links to candidate ads by state, office and congressional district
          </Link>
        </li>
      </ul>
    </div>
  </div>
);

export default AdminTools;
