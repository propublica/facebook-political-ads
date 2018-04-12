import React from "react";
import { connect } from "react-redux";
import { withRouter, Link } from "react-router-dom";
import { getGroupedAttrs, newSearch } from "actions.js";

// maps groupingType to a URL function
const groupedAttrUrl = {
  target: ({ target }) => `targets=%5B%7B"target"%3A"${target}"%7D%5D`,
  advertiser: ({ advertiser }) => `advertisers=%5B"${advertiser}"%5D`,
  segment: ({ segment }) => {
    if (segment === "List → " || segment === "Like  → ") {
      return `targets=%5B%7B"target"%3A"${segment.slice(0, 4)}"%7D%5D`;
    } else {
      const split = segment.split(" → ");
      return `targets=%5B%7B"target"%3A"${split[0]}","segment"%3A"${
        split[1]
      }"%7D%5D`;
    }
  }
};

export class GroupedAttrsUnconnected extends React.Component {
  componentWillMount() {
    // TODO: follow this pattern to get various kinds of grouped attrs (advertisers, recent advertisers, etc.)
    let groupingType = "advertiser"; // default
    let recent = false;
    if (this.props.match) {
      // `match` is from React Router -- it's the bit of the URL that matches.
      groupingType = this.props.match.params.groupingType;
      recent = this.props.match.params.timing;
      // the varieties of allowed groupingTypes are defined in Rust, in server.rs
      this.setState({ groupingType: groupingType });
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
                    to={`/facebook-ads/admin/ads?${groupedAttrUrl[
                      this.state.groupingType
                    ](groupedItem)}`}
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
