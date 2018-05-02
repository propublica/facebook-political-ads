import React from "react";
import { connect } from "react-redux";
import { withRouter } from "react-router-dom";
import { getSummary, setLang } from "actions.js";
import i18next from "i18next";
import PropTypes from "prop-types";
import { Line } from "nivo";

export class SummaryUnconnected extends React.Component {
  componentWillMount() {
    const params = new URLSearchParams(location.search);
    this.props.setLang(
      this.props.lang || params.get("lang") || i18next.language
    );
    this.props.getSummary();
  }

  render() {
    // ads_this_week: ads_this_week,
    // ads_today: ads_today,
    // daily_political_ratio: daily_political_ratio,
    // weekly_political_ratio: weekly_political_ratio,
    // last_received_at: last_received_at
    // total_political_ads: political_ads_count,

    var width = 500,
      height = 300,
      margins = { left: 100, right: 100, top: 50, bottom: 50 },
      // chart series,
      // field: is what field your data want to be selected
      // name: the name of the field that display in legend
      // color: what color is the line
      // your x accessor
      x = function(d) {
        return parseDate(d[0]);
      },
      xScale = "time";

    var daily_chart_data =
      this.props.summary && this.props.summary.daily_political_ratio
        ? [
            {
              id: "daily",
              data: this.props.summary.daily_political_ratio.map(a => ({
                x: a[0].slice(5),
                y: a[1]
              }))
            }
          ]
        : [];
    /* I am gonna make it ... */ var this_year = new Date().getFullYear();
    var weekly_chart_data =
      this.props.summary && this.props.summary.weekly_political_ratio
        ? [
            {
              id: "weekly",
              data: this.props.summary.weekly_political_ratio.map(a => ({
                x: new Date(this_year, 0, 1 + (a[0] - 1) * 7)
                  .toISOString()
                  .slice(5, 10),
                y: a[1]
              }))
            }
          ]
        : [];
    console.log(weekly_chart_data);
    var daily_chart_or_not =
      this.props.summary && this.props.summary.daily_political_ratio ? (
        <Line
          margin={margins}
          data={daily_chart_data}
          stacked={false}
          width={width}
          height={height}
          axisBottom={{
            orient: "bottom",
            tickSize: 5,
            tickPadding: 5,
            tickRotation: 0,
            legend: "date",
            legendOffset: 36,
            legendPosition: "center"
          }}
        />
      ) : (
        <div />
      );
    var weekly_chart_or_not =
      this.props.summary && this.props.summary.weekly_political_ratio ? (
        <Line
          margin={margins}
          data={weekly_chart_data}
          stacked={false}
          width={width}
          height={height}
          axisBottom={{
            orient: "bottom",
            tickSize: 5,
            tickPadding: 5,
            tickRotation: 0,
            legend: "date",
            legendOffset: 36,
            legendPosition: "center"
          }}
        />
      ) : (
        <div />
      );

    return this.props.summary ? (
      <div id="summary">
        <div className="last-ad">
          Last ad received at:{" "}
          {new Date(Date.parse(this.props.summary.last_received_at)).toString()}
        </div>
        <div className="thirds">
          <figure
            className="number number--large today col"
            data-enhanced-number="true"
          >
            <p>
              <strong>{this.props.summary.ads_today}</strong> political ads
              today
            </p>
          </figure>
          <figure
            className="number number--large thisweek col"
            data-enhanced-number="true"
          >
            <p>
              <strong>{this.props.summary.ads_this_week}</strong> political ads
              this week
            </p>
          </figure>
          <figure
            className="number number--large total col"
            data-enhanced-number="true"
          >
            <p>
              <strong>{this.props.summary.total_political_ads}</strong> total
              political ads
            </p>
          </figure>
          <div className="clearboth" />
        </div>
        <div className="halves">
          <div className="col">
            <h3>Ratio of political ads to all ads, by day, past week</h3>
            {daily_chart_or_not}
          </div>
          <div className="col">
            <h3>Ratio of political ads to all ads, by week, past few months</h3>
            {weekly_chart_or_not}
          </div>
        </div>
      </div>
    ) : (
      "loading..."
    );
  }
}

const Summary = withRouter(
  connect(
    ({ summary, lang }) => ({
      summary,
      lang
    }),
    dispatch => ({
      getSummary: () => dispatch(getSummary()),
      setLang: lang => dispatch(setLang(lang))
    })
  )(SummaryUnconnected)
);
export default Summary;
