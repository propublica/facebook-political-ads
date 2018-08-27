import React from "react";
import * as d3 from "d3";
import { withFauxDOM } from "react-faux-dom";

export class PoliticalAdsChartUnfauxed extends React.Component {
  componentDidMount() {
    const faux = this.props.connectFauxDOM("div", "chart");
    const $faux = d3.select(faux);
    const svg = $faux.append("svg");

    // magic for being responsive.
    var container = document.getElementById("adstoday");
    var containerWidth = container.getBoundingClientRect()["width"];
    var containerHeight = container.getBoundingClientRect()["height"];
    svg.attr("width", "100%");
    svg.attr("height", "100%");
    svg.attr("viewBox", "0 0 " + containerWidth + " " + containerHeight);
    svg.attr("preserveAspectRatio", "xMinYMin");

    let margin = { left: 25, right: 25, top: 10, bottom: 5 };
    let width = containerWidth - margin.left - margin.right;
    let height = containerHeight - margin.top - margin.bottom;
    let g = svg
      .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var x = d3.scaleTime().rangeRound([0, width]);

    var y = d3.scaleLinear().rangeRound([height, 0]);

    /* I am gonna make it ... */ var this_year /* if it kills me */ = new Date().getFullYear();
    var political_ads_per_day_chart_data = this.props.political_ads_per_day
      ? this.props.political_ads_per_day.slice(1).map(a => {
          return {
          date: new Date(this_year, 0, (a[0] - 2) * 7 + 1),
          cnt: a[1]
        };
      })
      : [];
    // var dotLabel;
    // if (idx == 0) {
    //   dotLabel = "Two weeks ago";
    // } else if (idx == this.props.political_ads_per_day.length - 1) {
    //   dotLabel = `${a[1]} ads collected this week`;
    // } else {
    //   dotLabel = 0;
    // }

    x.domain(
      d3.extent(political_ads_per_day_chart_data, function(d) {
        return d.date;
      })
    );
    y.domain([
      0,
      d3.max(political_ads_per_day_chart_data, function(d) {
        return d.cnt;
      })
    ]);

    var line = d3
      .line()
      .x(function(d) {
        return x(d.date);
      })
      .y(function(d) {
        return y(d.cnt);
      });

    // var area = d3
    //   .area()
    //   .x(function(d) {
    //     return x(d.date);
    //   })
    //   .y0(height)
    //   .y1(function(d) {
    //     return y(d.cnt);
    //   });
    // g
    //   .append("path")
    //   .datum(political_ads_per_day_chart_data)
    //   .attr("fill", "#CCE8F8")
    //   .attr("stroke", "none")
    //   .attr("class", "area")
    //   .attr("d", area);

    g
      .append("path")
      .datum(political_ads_per_day_chart_data)
      .attr("fill", "none")
      .attr("stroke", "#0964a9")
      .attr("stroke-linejoin", "round")
      .attr("stroke-linecap", "round")
      .attr("stroke-width", 1.5)
      .attr("d", line);

    g
      .selectAll(".circle")
      .data(political_ads_per_day_chart_data)
      .enter()
      .append("circle")
      .attr("class", "circle")
      .attr("cx", function(d) {
        return x(d.date);
      })
      .attr("cy", function(d) {
        return y(d.cnt);
      })
      .attr("r", function(d, idx) {
        return idx == political_ads_per_day_chart_data.length - 1 || idx == 0
          ? 3
          : 0;
      })
      .attr("fill", "#0964a9");

    let last_data_point =
      political_ads_per_day_chart_data.length == 0
        ? []
        : [
            political_ads_per_day_chart_data[
              political_ads_per_day_chart_data.length - 1
            ]
          ];
    let first_data_point =
      political_ads_per_day_chart_data.length == 0
        ? []
        : [political_ads_per_day_chart_data[0]];

    /* count */ g
      .selectAll(".annotation-count")
      .data(last_data_point)
      .enter()
      .append("text")
      .attr("class", "annotation-count")
      .attr("x", function(d) {
        return x(d.date);
      })
      .attr("y", 40)
      .attr("dx", "-10px")
      .attr("font-size", 16)
      .attr("text-anchor", "end")
      .attr("font-weight", "bold")
      .text(d => d.cnt.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","));

    /* jan 1 */
    g
      .selectAll(".annotation-startdate")
      .data(first_data_point)
      .enter()
      .append("text")
      .attr("class", "annotation-startdate")
      .attr("x", function(d) {
        return x(d.date);
      })
      .attr("y", function(d) {
        return 33;
      })
      .attr("dy", "7px")
      .attr("font-size", 9)
      .attr("text-anchor", "middle")
      .text(d => `${d3.timeFormat("%b. %-d")(d.date)}`);

    // todays date
    g
      .selectAll(".annotation-enddate")
      .data(last_data_point)
      .enter()
      .append("text")
      .attr("class", "annotation-enddate")
      .attr("x", function(d) {
        return x(d.date);
      })
      .attr("y", function(d) {
        return y(d.cnt);
      })
      .attr("dy", "14px")
      .attr("dx", "15px")
      .attr("font-size", 9)
      .attr("text-anchor", "end")
      .text(
        (d, idx) =>
          idx == political_ads_per_day_chart_data.length - 1
            ? "total"
            : idx == 0 ? `${d3.timeFormat("%b. %-d")(new Date())}` : ""
      );

    /* "total" */
    g
      .selectAll(".annotation-total")
      .data(last_data_point)
      .enter()
      .append("text")
      .attr("class", "annotation-total")
      .attr("x", function(d) {
        return x(d.date);
      })
      .attr("y", 40)
      .attr("dx", "15px")
      .attr("font-size", 10)
      .attr("text-anchor", "end")
      .text("total");

    // if (last_data_point.length > 0) {
    //   g
    //     .append("line")
    //     .attr("class", "annotation-line")
    //     .attr("stroke", "gray")
    //     .attr("stroke-width", 1)
    //     .attr("x1", x(last_data_point[0].date))
    //     .attr("x2", x(last_data_point[0].date))
    //     .attr("y1", y(24000))
    //     .attr("y2", y(last_data_point[0].cnt));
    // }
  }

  render() {
    return (
      <div id="adstoday">
        <div className="renderedD3">{this.props.chart}</div>
      </div>
    );
  }
}

PoliticalAdsChartUnfauxed.defaultProps = {
  chart: "loading..."
};

const PoliticalAdsChart = withFauxDOM(PoliticalAdsChartUnfauxed);
export default PoliticalAdsChart;

// <ResponsiveLine
// className="adstoday"
// data={political_ads_per_day_chart_data}
// stacked={false}
// margin={margins}
// height={70}
// isInteractive={false}
// animate={false}
// colors={["#49b3e7"]}
// lineWidth={2}
// enableGridX={false}
// enableGridY={false}
// dotSize={2}
// dotBorderWidth={0}
// enableDotLabel={true}
// dotLabel={val => val.x}
// dotLabelYOffset={-10}
// dotLabelXOffset={15}
// axisLeft={null}
// axisBottom={null}
// />
