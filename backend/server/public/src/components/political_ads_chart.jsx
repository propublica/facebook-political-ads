import React from "react";
import PropTypes from "prop-types"; // magic
import { ResponsiveLine } from "nivo";

export default class PoliticalAdsChart extends React.Component {
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
                key: idx == 0 || idx == this.props.political_ads_per_day.length - 1 // oddly required for the dotLabel thing below.
            };
          })
          }
        ]
      : [];

    var margins = { left: 0, right: 0, top: 20, bottom: 30 };

    var ads_today_chart_or_not = this.props.political_ads_per_day ? (
      <ResponsiveLine
        data={political_ads_per_day_chart_data}
        stacked={false}
        margin={margins}
        height={150}
        isInteractive={false}
        animate={false}
        colors={["#49b3e7"]}
        lineWidth={2}
        enableGridX={false}
        enableGridY={false}
        dotSize={2}
        dotBorderWidth={0}
        enableDotLabel={true}
        dotLabel={val => {
          return val.x ? `${val.y} ads collected this week` : "";
        }}
        dotLabelYOffset={-12}
        dotLabelXOffset={30}
        theme={{
          axis: {
            fontSize: "9px"
          }
        }}
        axisLeft={null}
        axisBottom={null}
      />
    ) : (
      <div />
    );
    return this.props.political_ads_per_day
      ? ads_today_chart_or_not
      : "loading...";
  }
}
