import React from "react";
import Targeting from "./targeting.jsx";
import { Link } from "react-router-dom";
import DOMPurify from "dompurify";
import { connect } from "react-redux";

export class AdUnconnected extends React.Component {
  constructor(props) {
    super(props);
    this.adRef = React.createRef();
  }
  componentDidMount() {
    if (!this.adRef || !this.adRef.current) return;
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
    return this.props.ad ? (
      <div className="ad" ref={this.adRef}>
        <div className="message">
          <div
            dangerouslySetInnerHTML={{
              __html: DOMPurify.sanitize(this.props.ad.html)
            }}
          />
        </div>
        <div className="ad-metadata">
          <Link
            className="permalink"
            to={`/facebook-ads/ad/${this.props.ad.id}`}
          >
            Permalink to this ad
          </Link>
          <p>
            First seen:{" "}
            <time dateTime="{this.props.ad.created_at}">
              {new Date(
                Date.parse(this.props.ad.created_at)
              ).toLocaleDateString("en-US", {
                day: "numeric",
                month: "long",
                year: "numeric"
              })}
            </time>
          </p>
        </div>
        {this.props.ad.targeting !== null ? (
          <Targeting targeting={this.props.ad.targeting} />
        ) : (
          ""
        )}
      </div>
    ) : null;
  }
}

const Ad = connect(({ ad }) => ad)(AdUnconnected);

export default Ad;
