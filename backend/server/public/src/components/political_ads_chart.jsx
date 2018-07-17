import React from "react";
import PropTypes from "prop-types"; // magic
import { ResponsiveLine } from "nivo";

export default class PoliticalAdsChart extends React.Component {
  componentDidUpdate() {
    setTimeout(() => {
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
        let dots = chart.getElementsByTagName("circle");
        dots[0].setAttribute("stroke-width", 4);
        dots[dots.length - 1].setAttribute("stroke-width", 4);
        dots[0].setAttribute("stroke", "#000");
        dots[dots.length - 1].setAttribute("stroke", "#000");

        let texts = chart.getElementsByTagName("text");
        let firstLabel = texts[0];
        let secondLabel = texts[1];
        firstLabel.setAttribute("text-anchor", "start");
        secondLabel.setAttribute("text-anchor", "end");
        let secondLabelFirstLine = document.createElementNS(xmlns, "text");
        secondLabelFirstLine.textContent = secondLabel.textContent.split(
          "collected"
        )[0];
        secondLabel.textContent = secondLabel.textContent.split("collected")[1];
        secondLabelFirstLine.setAttribute("text-anchor", "end");
        secondLabelFirstLine.setAttribute(
          "style",
          "font-size: 11px; fill: rgb(0, 0, 0);"
        );
        secondLabelFirstLine.setAttribute(
          "y",
          parseInt(secondLabel.getAttribute("y")) - 20
        );
        secondLabel.parentNode.appendChild(secondLabelFirstLine);

        let secondLabelSecondLine = document.createElementNS(xmlns, "text");
        secondLabelSecondLine.textContent = "collected";
        secondLabelSecondLine.setAttribute("text-anchor", "end");
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
        firstLabelFirstLine.textContent =
          firstLabel.textContent.split("weeks")[0] + "weeks";
        firstLabel.textContent = firstLabel.textContent.split("weeks")[1];
        firstLabelFirstLine.setAttribute("text-anchor", "start");
        firstLabelFirstLine.setAttribute(
          "style",
          "font-size: 11px; fill: rgb(0, 0, 0);"
        );
        firstLabelFirstLine.setAttribute(
          "y",
          parseInt(firstLabel.getAttribute("y")) - 7
        );
        firstLabel.parentNode.appendChild(firstLabelFirstLine);
        firstLabel.setAttribute(
          "y",
          parseInt(firstLabel.getAttribute("y")) + 3
        );
      }
    }, 10);
  }
  render() {
    /* I am gonna make it ... */ var this_year = new Date().getFullYear();
    var political_ads_per_day_chart_data = this.props.political_ads_per_day
      ? [
        {
          id: "weekly",
          data: this.props.political_ads_per_day.map((a, idx) => {
            // console.log(
            //   new Date(this_year, 0, 1 + (a[0] - 1) * 7)
            //     .toISOString()
            //     .slice(5, 10)
            //     .replace("-", "/")
            //   .replace(/^0/, "")
            // );
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

    var margins = { left: 20, right: 30, top: 20, bottom: 0 };

    var ads_today_chart_or_not = this.props.political_ads_per_day ? (
      <div
        id="adstoday"
        style={{
          position: "relative",
          height: "100%",
          left: "-30px",
          width: "calc(100% + 60px)"
        }}
      >
        <ResponsiveLine
          className="adstoday"
          data={political_ads_per_day_chart_data}
          stacked={false}
          margin={margins}
          height={80}
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
