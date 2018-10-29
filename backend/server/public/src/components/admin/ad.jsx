import React from "react";
import { connect } from "react-redux";
import { suppressAd } from "actions.js";
import { Link } from "react-router-dom";
import DOMPurify from "dompurify";

// export const AdminAdUnconnected = ({ ad, onSuppressClick }) => (
export class AdminAdUnconnected extends React.Component {
  constructor(props) {
    super(props);
    this.adRef = React.createRef();
  }
  componentDidMount() {
    const link = this.adRef.current.querySelector(".see_more_link");
    if (!link) return;
    link.addEventListener("click", () => {
      this.adRef.current.querySelector(".text_exposed_hide").style.display =
        "none";
      this.adRef.current.querySelector(".see_more_link").style.display = "none";
      this.adRef.current
        .querySelectorAll(".text_exposed_show")
        .forEach(node => (node.style.display = "inline"));
    });
  }
  render() {
    return (
      <div className="ad" ref={this.adRef}>
        <table>
          <tbody>
            <tr>
              <td>id</td>
              <td>
                <Link to={`/facebook-ads/admin/ads/${this.props.ad.id}`}>
                  {this.props.ad.id}
                </Link>{" "}
                ({this.props.ad.lang}){" "}
                {this.props.ad.is_yougov ? "(YouGov ad)" : ""}
              </td>
            </tr>
            <tr>
              <td>first seen</td>
              <td>
                {new Date(Date.parse(this.props.ad.created_at)).toString()}
              </td>
            </tr>
            <tr>
              <td>title / paid for by </td>
              <td>
                {this.props.ad.title} / Paid for by:{" "}
                {this.props.ad.paid_for_by ? this.props.ad.paid_for_by : "n/a"}
              </td>
            </tr>
            <tr>
              <td>ad</td>
              <td
                className="message"
                dangerouslySetInnerHTML={{
                  __html: DOMPurify.sanitize(this.props.ad.html)
                }}
              />
            </tr>
            <tr>
              <td>targeting</td>
              <td
                dangerouslySetInnerHTML={{
                  __html: DOMPurify.sanitize(this.props.ad.targeting)
                }}
              />
            </tr>
            <tr>
              <td>parsed targets</td>
              <td>
                {this.props.ad.targets && this.props.ad.targets.length > 0
                  ? this.props.ad.targets
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
                {this.props.ad.political} / {this.props.ad.not_political}
              </td>
            </tr>
            <tr>
              <td>political likelihood</td>
              <td>
                {parseInt(this.props.ad.political_probability * 10000) / 100}%
              </td>
            </tr>
            <tr>
              <td>impressions</td>
              <td>{this.props.ad.impressions}</td>
            </tr>

            {this.props.ad.lang == "en-US" ? (
              <tr>
                <td>More ads by this advertiser</td>
                <td>
                  <a
                    href={`https://www.facebook.com/politicalcontentads/?active_status=all&q=${
                      this.props.ad.advertiser
                    }`}
                  >
                    Facebook Political Ad Archive
                  </a>{" "}
                  |{" "}
                  <a
                    href={`/facebook-ads/admin?advertisers=%5B%22${
                      this.props.ad.advertiser
                    }%22%5D`}
                  >
                    FBPAC ads
                  </a>{" "}
                  |{" "}
                  <a
                    href={`/fbpac-api/ads/advertiser?advertiser=${this.props.ad
                      .advertiser || this.props.ad.title}`}
                  >
                    Breakdown of Targeting Choices
                  </a>{" "}
                  |{" "}
                  {this.props.ad.political_probability > 0.7 ? (
                    <a href={`/facebook-ads/ad/${this.props.ad.id}`}>
                      Public Permalink
                    </a>
                  ) : null}
                </td>
              </tr>
            ) : null}

            <tr>
              <td colSpan="2">
                {this.props.ad.suppressed ? (
                  "Suppressed"
                ) : (
                  <button
                    onClick={() => this.props.onSuppressClick(this.props.ad)}
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
  }
}

const AdminAd = connect(
  ({ ad }) => ad,
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
