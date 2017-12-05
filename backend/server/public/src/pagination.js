import { h } from 'preact';
import { connect } from 'preact-redux';
import { range } from 'lodash';

const NEXT_PAGE = 'next_page';
const PREV_PAGE = 'prev_page';
const SET_PAGE = 'set_page';
const SET_TOTAL = 'set_total';
const PER_PAGE = 20;
const nextPage = () => ({ type: NEXT_PAGE });
const prevPage = () => ({ type: PREV_PAGE });
const setPage = (page) => ({ type: SET_PAGE, value: page });
const setTotal = (total) => ({ type: SET_TOTAL, value: total });

const min = (state) => Math.min(state.page, state.total);
const pagination = (state = { page: 0, total: 0 }, action) => {
  switch(action.type) {
  case NEXT_PAGE:
    return { ...state, page: min({ ...state, page: state.page + 1 }) };
  case PREV_PAGE:
    return { ...state, page: Math.max(state.page - 1, 0) };
  case SET_PAGE:
    return { ...state, page: min({ page: action.value, total: state.total })};
  case SET_TOTAL: {
    const total = Math.ceil(action.value / PER_PAGE);
    return { total, page: min({ total: total, page: state.page }) };
  }
  default:
    return state;
  }
};

let Pagination = ({ page, prev, next, set, total }) => {
  if(total === 0) return (<p className="no_ads">No results for your query.</p>);
  return (<nav className="pagination">
    <ul>
      { page > 0 ? <li><a href="" onClick={prev}>←</a></li> : ''}
      {range(Math.max(0, page - 2), min({ page: page + 3, total: total + 1 })).map((i) => {
        return (i === page ?
          <li key={i} className="current">{page + 1}</li> :
          <li key={i}><a href="" onClick={(e) => set(e, i)}>{i + 1}</a></li>);
      })}
      { page + 1 < total ? <li><a href="" onClick={next}>→</a></li> : ''}
    </ul>
  </nav>);
};
Pagination = connect(
  ({ pagination }) => pagination,
  (dispatch) => ({
    prev: (e) => {
      e.preventDefault();
      dispatch(prevPage());
    },
    next: (e) => {
      e.preventDefault();
      dispatch(nextPage());
    },
    set: (e, i) => {
      e.preventDefault();
      dispatch(setPage(i));
    }
  })
)(Pagination);

export { Pagination, pagination, setTotal, setPage };
