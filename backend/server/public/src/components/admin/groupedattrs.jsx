import React from "react";
import { connect } from "react-redux";
import { withRouter, Link } from "react-router-dom";
import { getGroupedAttrs, newSearch } from "actions.js";

export class GroupedAttrsUnconnected extends React.Component {
  componentDidMount() {
    // TODO: follow this pattern to get various kinds of grouped attrs (advertisers, recent advertisers, etc.)
    let groupingType = "advertisers"; // default
    if (this.props.match) {
      // `match` is from React Router -- it's the bit of the URL that matches.
      groupingType = this.props.match.params.groupingType
        .replace("recent_", "")
        .replace("recent", "");
      // the varieties of allowed groupingTypes are defined in Rust, in server.rs
      this.setState({ groupingType: groupingType });
    }
    console.log(this.state);
    this.props.getGroupedAttrs(groupingType);
  }

  render() {
    return (
      <table id="advertisers" className="breakdown">
        <thead>
          <tr>
            <th>
              {this.state
                ? this.state.groupingType[0].toUpperCase() +
                  this.state.groupingType.substr(1)
                : ""}
            </th>
            <th>Count</th>
          </tr>
        </thead>
        <tbody>
          {this.props.groupedAttribute.map(advertiser => (
            <tr key={advertiser[this.state.groupingType]}>
              <td>
                <Link
                  to={`/facebook-ads/admin/ads?search=${
                    advertiser[this.state.groupingType]
                  }&${this.state.groupingType}s=%5B"${
                    advertiser[this.state.groupingType]
                  }"%5D`}
                >
                  {advertiser[this.state.groupingType]}
                </Link>
              </td>
              <td>{advertiser.count}</td>
            </tr>
          ))}
        </tbody>
      </table>
    );
  }
}

const GroupedAttrs = withRouter(
  connect(
    ({ groupedAttribute }) => ({
      groupedAttribute
    }),
    dispatch => ({
      onClick: term => dispatch(newSearch(term)),
      getGroupedAttrs: kind => dispatch(getGroupedAttrs(kind))
    })
  )(GroupedAttrsUnconnected)
);
export default GroupedAttrs;
