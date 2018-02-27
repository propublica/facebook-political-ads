import React from "react";
import { connect } from "react-redux";
import { range } from "lodash";
import { fetchPrevPage, fetchNextPage, fetchPage } from "actions.js";

export const PaginationUnconnected = ({ page, prev, next, set, total }) => (
  <nav className="pagination">
    <ul>
      {page > 0 ? (
        <li>
          <a href="" onClick={prev}>
            ←
          </a>
        </li>
      ) : (
        ""
      )}
      {range(Math.max(0, page - 2), Math.min(page + 3, total)).map(i => {
        return i === page ? (
          <li key={i} className="current">
            {page + 1}
          </li>
        ) : (
          <li key={i}>
            <a href="" onClick={e => set(e, i)}>
              {i + 1}
            </a>
          </li>
        );
      })}
      {page + 1 < total ? (
        <li>
          <a href="" onClick={next}>
            →
          </a>
        </li>
      ) : (
        ""
      )}
    </ul>
  </nav>
);
const Pagination = connect(
  ({ pagination }) => pagination,
  dispatch => ({
    prev: e => {
      e.preventDefault();
      dispatch(fetchPrevPage());
    },
    next: e => {
      e.preventDefault();
      dispatch(fetchNextPage());
    },
    set: (e, i) => {
      e.preventDefault();
      dispatch(fetchPage(i));
    }
  })
)(PaginationUnconnected);
export default Pagination;
