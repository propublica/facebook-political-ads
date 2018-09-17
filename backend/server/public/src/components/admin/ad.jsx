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
            <Link to={`/facebook-ads/admin/ads/${ad.id}`}>{ad.id}</Link> ({
              ad.lang
            })
          </td>
        </tr>
        <tr>
          <td>first seen</td>
          <td>{new Date(Date.parse(ad.created_at)).toString()}</td>
        </tr>
        <tr>
          <td>title / paid for by </td>
          <td>
            {ad.title} / Paid for by: {ad.paid_for_by ? ad.paid_for_by : "n/a"}
          </td>
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
            {ad.targets.length > 0
              ? ad.targets
                .map(
                    ({ target, segment }) =>
                      segment ? (
                        <span key={`${target}${segment}`}>
                          <a
                            href={`/facebook-ads/admin/ads?targets=%5B%7B%22target%22%3A%22${target}%22%7D%5D`}
                        >
                            {target}
                          </a>:{" "}
                          <a
                            href={`/facebook-ads/admin/ads?targets=%5B%7B%22target%22%3A%22${target}%22%2C%22segment%22%3A%22${segment}%22%7D%5D`}
                          >
                            {segment}
                          </a>
                        </span>
                      ) : (
                        <a
                        key={target}
                          href={`/facebook-ads/admin/ads?targets=%5B%7B%22target%22%3A%22${target}%22%7D%5D`}
                      >
                          {target}
                        </a>
                      )
                  )
                .reduce((prev, curr) => [prev, ", ", curr])
              : "NONE"}
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

        {ad.lang == "en-US" ? (
          <tr>
            <td>More ads by this advertiser</td>
            <td>
              <a
                href={`https://www.facebook.com/politicalcontentads/?active_status=all&q=${
                  ad.advertiser
                }`}
              >
                Facebook Political Ad Archive for {ad.advertiser}
              </a>{" "}
              |{" "}
              <a
                href={`/facebook-ads/admin?advertisers=%5B%22${
                  ad.advertiser
                }%22%5D`}
              >
                FBPAC ads by {ad.advertiser}
              </a>
            </td>
          </tr>
        ) : null}

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
        confirm("Are you sure that you want to mark this ad as NOT POLITICAL?")
      ) {
        dispatch(suppressAd(ad));
      }
    }
  })
)(AdminAdUnconnected);

export default AdminAd;
