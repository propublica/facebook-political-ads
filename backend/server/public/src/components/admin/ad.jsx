import React from "react";
import { connect } from "react-redux";
import { suppressAd } from "actions.js";
import { Link } from "react-router-dom";

export const AdminAdUnconnected = ({ ad, onSuppressClick }) => (
  <div className="ad">
    <table>
      <tbody>
        <tr>
          <td>id</td>
          <td>
            <Link to={`/facebook-ads/admin/ads/${ad.id}`}>{ad.id}</Link>
          </td>
        </tr>
        <tr>
          <td>first seen</td>
          <td>{new Date(Date.parse(ad.created_at)).toString()}</td>
        </tr>
        <tr>
          <td>title</td>
          <td>{ad.title}</td>
        </tr>
        <tr>
          <td>ad</td>
          <td
            className="message"
            dangerouslySetInnerHTML={{ __html: ad.html }}
          />
        </tr>
        <tr>
          <td>targeting</td>
          <td dangerouslySetInnerHTML={{ __html: ad.targeting }} />
        </tr>
        <tr>
          <td>parsed targets</td>
          <td>
            {ad.targets
              .map(({ target, segment }) => `${target}: ${segment}`)
              .join(", ")}
          </td>
        </tr>
        <tr>
          <td>political / not political</td>
          <td>
            {ad.political} / {ad.not_political}
          </td>
        </tr>
        <tr>
          <td>political likelihood</td>
          <td>{parseInt(ad.political_probability * 10000) / 100}%</td>
        </tr>
        <tr>
          <td>impressions</td>
          <td>{ad.impressions}</td>
        </tr>
        <tr>
          <td colSpan="2">
            {ad.suppressed ? (
              "Suppressed"
            ) : (
              <button
                onClick={function() {
                  return onSuppressClick(ad);
                }}
              >
                Suppress (Is this ad not political?)
              </button>
            )}
          </td>
        </tr>
      </tbody>
    </table>
  </div>
);

const AdminAd = connect(
  () => ({}),
  dispatch => ({
    onSuppressClick: ad => {
      if (
        confirm("Are you sure that you want to mark this ad is NOT POLITICAL?")
      ) {
        dispatch(suppressAd(ad));
      }
    }
  })
)(AdminAdUnconnected);

export default AdminAd;
