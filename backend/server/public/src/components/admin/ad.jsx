import React from "react";
import { connect } from "react-redux";
import { getOneAd, suppressAd } from "actions.js";

let AdminAd = ({ ad, onSuppressClick, onPermalinkClick }) => (
  <div className="ad">
    <table>
      <tbody>
        <tr>
          <td>id</td>
          <td>
            <a
              href={"?detail=" + ad.id}
              onClick={e => {
                e.preventDefault();
                onPermalinkClick(ad.id);
              }}
            >
              {ad.id}
            </a>
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
          <td>text</td>
          <td dangerouslySetInnerHTML={{ __html: ad.html }} />
        </tr>
        <tr>
          <td>targeting</td>
          <td dangerouslySetInnerHTML={{ __html: ad.targeting }} />
        </tr>
        <tr>
          <td>political / not political</td>
          <td>
            {ad.political} / {ad.not_political}
          </td>
        </tr>
        <tr>
          <td>likelihood</td>
          <td>{ad.political_probability}</td>
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
                Suppress
              </button>
            )}
          </td>
        </tr>
      </tbody>
    </table>
  </div>
);

AdminAd = connect(
  () => {},
  dispatch => ({
    onSuppressClick: ad => dispatch(suppressAd(ad)),
    onPermalinkClick: id => dispatch(getOneAd(id))
  })
)(AdminAd);

export default AdminAd;
