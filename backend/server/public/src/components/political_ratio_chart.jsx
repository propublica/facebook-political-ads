import React from "react";
import PropTypes from "prop-types"; // magic
import { ResponsiveLine } from "nivo";

export default class PoliticalRatioChart extends React.Component {
  render() {
    /* I am gonna make it ... */ var this_year = new Date().getFullYear();
    var weekly_ratio_chart_data = this.props.weekly_political_ratio
      ? [
          {
            id: "weekly",
            data: this.props.weekly_political_ratio.map(a => ({
              x: new Date(this_year, 0, 1 + (a[0] - 1) * 7)
                .toISOString()
                .slice(5, 10)
                .replace("-", "/")
              .replace(/^0/, ""),
              y: a[1]
            }))
          }
        ]
      : [];

    var margins = { left: 10, right: 10, top: 20, bottom: 30 };

    var weekly_ratio_chart_or_not = this.props.weekly_political_ratio ? (
      <ResponsiveLine
        data={weekly_ratio_chart_data}
        stacked={false}
        margin={margins}
        height={150}
        isInteractive={false}
        animate={false}
        colors={["#217ce3"]}
        enableGridX={false}
        enableGridY={false}
        enableDots={false}
        theme={{
          axis: {
            fontSize: "9px"
          }
        }}
        axisLeft={null}
        axisBottom={{
          orient: "bottom",
          tickSize: 5,
          tickPadding: 5,
          tickRotation: 0,
          // legend: "date",
          legendOffset: 36,
          legendPosition: "center"
        }}
      />
    ) : (
      <div />
    );
    return this.props.weekly_political_ratio
      ? weekly_ratio_chart_or_not
      : "loading...";
  }
}
