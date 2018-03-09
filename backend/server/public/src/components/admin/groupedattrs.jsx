import React from "react";
import { connect } from "react-redux";
import { withRouter, Link } from "react-router-dom";
import { getGroupedAttrs, newSearch, RECENT } from "actions.js";

export class GroupedAttrsUnconnected extends React.Component {
  componentWillMount() {
    // TODO: follow this pattern to get various kinds of grouped attrs (advertisers, recent advertisers, etc.)
    let groupingType = "advertiser"; // default
    let recent = false;
    if (this.props.match) {
      // `match` is from React Router -- it's the bit of the URL that matches.
      groupingType = this.props.match.params.groupingType
        .replace("recent_", "")
        .replace("by_", "");
      // the varieties of allowed groupingTypes are defined in Rust, in server.rs
      this.setState({ groupingType: groupingType });
      recent =
        this.props.match.params.groupingType.indexOf("recent") >= 0
          ? RECENT
          : null;
    }
    this.props.getGroupedAttrs(groupingType, recent);
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
          {this.props.groupedAttribute ? (
            this.props.groupedAttribute.map(groupedItem => (
              <tr key={groupedItem[this.state.groupingType]}>
                <td>
                  <Link
                    to={`/facebook-ads/admin/ads?${
                      this.state.groupingType
                    }s=%5B"${groupedItem[this.state.groupingType]}"%5D`}
                  >
                    {groupedItem[this.state.groupingType]}
                  </Link>
                </td>
                <td>{groupedItem.count}</td>
              </tr>
            ))
          ) : (
            <tr>
              <td>Loading...</td>
            </tr>
          )}
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
      getGroupedAttrs: (kind, recent) => dispatch(getGroupedAttrs(kind, recent))
    })
  )(GroupedAttrsUnconnected)
);
export default GroupedAttrs;
