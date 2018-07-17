import React from "react";
import PropTypes from "prop-types"; // magic
import { ResponsiveLine } from "nivo";

export default class PoliticalAdsChart extends React.Component {
  fixChart() {
    // this the worst thing I've ever written.
    // but Nivo is insufficiently flexible
    // so we're modifying the labels
    // and setting the dot widths of just the first/last points
    // via RAW UNADULTERATED JAVASCRIPT
    var xmlns = "http://www.w3.org/2000/svg";

    let chartContainer = document.getElementById("adstoday");
    if (!chartContainer) return;
    let chart = chartContainer.getElementsByTagName("svg")[0];
    if (chart) {
      let preexistingannots = Array.from(chart.getElementsByClassName("extra"));
      preexistingannots.forEach(el => el.remove());
      let dots = chart.getElementsByTagName("circle");
      dots[0].setAttribute("stroke-width", 4);
      dots[dots.length - 1].setAttribute("stroke-width", 4);
      dots[0].setAttribute("stroke", "#000");
      dots[dots.length - 1].setAttribute("stroke", "#000");

      let texts = chart.getElementsByTagName("text");
      let firstLabel = texts[0];
      let secondLabel = texts[1];
      secondLabel.setAttribute("y", 15);
      secondLabel.setAttribute("x", 5);
      secondLabel.setAttribute(
        "original-text",
        secondLabel.getAttribute("original-text") || secondLabel.textContent
      );
      firstLabel.setAttribute(
        "original-text",
        firstLabel.getAttribute("original-text") || firstLabel.textContent
      );
      firstLabel.setAttribute("text-anchor", "start");
      secondLabel.setAttribute("text-anchor", "start");
      let secondLabelFirstLine = document.createElementNS(xmlns, "text");
      secondLabelFirstLine.setAttribute("class", "extra");
      secondLabelFirstLine.textContent = secondLabel
        .getAttribute("original-text")
        .split("collected")[0];
      secondLabel.textContent = secondLabel
        .getAttribute("original-text")
        .split("collected")[1];
      secondLabelFirstLine.setAttribute("text-anchor", "start");
      secondLabelFirstLine.setAttribute(
        "style",
        "font-size: 11px; fill: rgb(0, 0, 0);"
      );
      secondLabelFirstLine.setAttribute(
        "y",
        parseInt(secondLabel.getAttribute("y")) - 20
      );
      secondLabelFirstLine.setAttribute("x", 5);
      secondLabel.parentNode.appendChild(secondLabelFirstLine);

      let secondLabelSecondLine = document.createElementNS(xmlns, "text");
      secondLabelSecondLine.setAttribute("class", "extra");
      secondLabelSecondLine.textContent = "collected";
      secondLabelSecondLine.setAttribute("x", 5);
      secondLabelSecondLine.setAttribute("text-anchor", "start");
      secondLabelSecondLine.setAttribute(
        "style",
        "font-size: 11px; fill: rgb(0, 0, 0);"
      );
      secondLabelSecondLine.setAttribute(
        "y",
        parseInt(secondLabel.getAttribute("y")) - 10
      );
      secondLabel.parentNode.appendChild(secondLabelSecondLine);

      // break text
      let firstLabelFirstLine = document.createElementNS(xmlns, "text");
      firstLabelFirstLine.setAttribute("class", "extra");
      firstLabelFirstLine.textContent =
        firstLabel.getAttribute("original-text").split("weeks")[0] + "weeks";
      firstLabel.textContent = firstLabel
        .getAttribute("original-text")
        .split("weeks")[1];
      firstLabelFirstLine.setAttribute("text-anchor", "start");
      firstLabelFirstLine.setAttribute(
        "style",
        "font-size: 11px; fill: rgb(0, 0, 0);"
      );
      firstLabel.setAttribute("y", -4);
      firstLabelFirstLine.setAttribute(
        "y",
        parseInt(firstLabel.getAttribute("y")) - 10
      );
      firstLabel.parentNode.appendChild(firstLabelFirstLine);
    }
  }

  componentDidMount() {
    window.addEventListener("resize", this.fixChart);
  }
  componentWillUnmount() {
    window.removeEventListener("resize", this.fixChart);
  }

  componentDidUpdate() {
    setTimeout(this.fixChart, 10);
    // setTimeout(this.fixChart, 50);
  }
  render() {
    /* I am gonna make it ... */ var this_year = new Date().getFullYear();
    var political_ads_per_day_chart_data = this.props.political_ads_per_day
      ? [
          {
            id: "weekly",
            data: this.props.political_ads_per_day.map((a, idx) => {
              var dotLabel;
              if (idx == 0) {
                dotLabel = "Two weeks ago";
              } else if (idx == this.props.political_ads_per_day.length - 1) {
                dotLabel = `${a[1]} ads collected this week`;
              } else {
                dotLabel = 0;
              }
              return {
              x:
                  new Date(this_year, 0, 1 + (a[0] - 1))
                    .toISOString()
                    .slice(5, 10)
                    .replace("-", "/")
                    .replace(/^0/, "") +
                  "_" +
                  idx,
                y: a[1],
                key: dotLabel // oddly required for the dotLabel thing below.
            };
          })
          }
        ]
      : [];

    var margins = { left: 20, right: 55, top: 10, bottom: 0 };

    var ads_today_chart_or_not = this.props.political_ads_per_day ? (
      <div
        id="adstoday"
        style={{
          position: "relative",
          // height: "calc(100% - 30px)",
          // top: "-30px",
          left: "-5%",
          width: "calc(100% + 10%)"
        }}
      >
        <ResponsiveLine
          className="adstoday"
          data={political_ads_per_day_chart_data}
          stacked={false}
          margin={margins}
          height={70}
          isInteractive={false}
          animate={false}
          colors={["#49b3e7"]}
          lineWidth={2}
          enableGridX={false}
          enableGridY={false}
          dotSize={2}
          dotBorderWidth={0}
          enableDotLabel={true}
          dotLabel={val => val.x}
          dotLabelYOffset={-10}
          dotLabelXOffset={15}
          axisLeft={null}
          axisBottom={null}
        />
      </div>
    ) : (
      <div />
    );
    return this.props.political_ads_per_day
      ? ads_today_chart_or_not
      : "loading...";
  }
}
