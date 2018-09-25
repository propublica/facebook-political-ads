import React from "react";
import { connect } from "react-redux";
import { range } from "lodash";
import { fetchPrevPage, fetchNextPage, fetchPage } from "actions.js";

export const PaginationUnconnected = ({
  page,
  prev,
  next,
  set,
  total,
  methodForPagination
}) => (
  <nav className="pagination">
    <ul>
      {page > 0 ? (
        <li>
          <a href="" onClick={e => prev(e, methodForPagination)}>
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
            <a href="" onClick={e => set(e, i, methodForPagination)}>
              {i + 1}
            </a>
          </li>
        );
      })}
      {page + 1 < total ? (
        <li>
          <a href="" onClick={e => next(e, methodForPagination)}>
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
    prev: (e, methodForPagination) => {
      e.preventDefault();
      dispatch(fetchPrevPage(methodForPagination));
    },
    next: (e, methodForPagination) => {
      e.preventDefault();
      dispatch(fetchNextPage(methodForPagination));
    },
    set: (e, i, methodForPagination) => {
      e.preventDefault();
      dispatch(fetchPage(i, methodForPagination));
    }
  })
)(PaginationUnconnected);
export default Pagination;
