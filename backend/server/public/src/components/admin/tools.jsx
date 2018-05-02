import React from "react";
import { Link } from "react-router-dom";

const AdminTools = () => (
  <div>
    <p>
      Here are some tools to hopefully help you find interesting and newsworthy
      ads.
    </p>
    <table className="tools">
      <tbody>
        <tr>
          <td>
            <Link to="/facebook-ads/admin/grouped/advertiser">
              All ads grouped by advertiser
            </Link>
          </td>
        </tr>
        <tr>
          <td>
            <Link to="/facebook-ads/admin/grouped/advertiser/this_month">
              Ads first submitted this month grouped by advertiser
            </Link>
          </td>
        </tr>
        <tr>
          <td>
            <Link to="/facebook-ads/admin/grouped/advertiser/this_week">
              Ads first submitted this week grouped by advertiser
            </Link>
          </td>
        </tr>

        <tr>
          <td>
            <Link to="/facebook-ads/admin/grouped/segment">
              All ads grouped by targeting segment
            </Link>
          </td>
          <td>
            (i.e. all ads targeted at people over 18, or all ads targeted by
            gender only to men)
          </td>
        </tr>
        <tr>
          <td>
            <Link to="/facebook-ads/admin/grouped/segment/this_month">
              Ads first submitted this month grouped by targeting segment
            </Link>
          </td>
          <td>
            (i.e. all ads targeted at people over 18, or all ads targeted by
            gender only to men)
          </td>
        </tr>
        <tr>
          <td>
            <Link to="/facebook-ads/admin/grouped/segment/this_week">
              Ads first submitted this week grouped by targeting segment
            </Link>
          </td>
          <td>
            (i.e. all ads targeted at people over 18, or all ads targeted by
            gender only to men)
          </td>
        </tr>

        <tr>
          <td>
            <Link to="/facebook-ads/admin/grouped/target">
              All ads grouped by targeting method
            </Link>
          </td>
          <td>(i.e. all ads targeted by age, or all ads targeted by gender)</td>
        </tr>
        <tr>
          <td>
            <Link to="/facebook-ads/admin/grouped/target/this_month">
              Ads first submitted this month grouped by targeting method
            </Link>
          </td>
          <td>(i.e. all ads targeted by age, or all ads targeted by gender)</td>
        </tr>
        <tr>
          <td>
            <Link to="/facebook-ads/admin/grouped/target/this_week">
              Ads first submitted this week grouped by targeting method
            </Link>
          </td>
          <td>(i.e. all ads targeted by age, or all ads targeted by gender)</td>
        </tr>

        <tr>
          <td>
            <Link to="/facebook-ads/admin/ads?search=Donald+J.+Trump&advertisers=%5B%22Donald+J.+Trump%22%5D">
              Trump ads
            </Link>
          </td>
        </tr>

        <tr>
          <td>
            <Link to="/facebook-ads/admin/summary">Summary Stats</Link>
          </td>
        </tr>
      </tbody>
    </table>
  </div>
);

export default AdminTools;
