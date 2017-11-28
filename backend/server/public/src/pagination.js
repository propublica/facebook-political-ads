const PAGE_NEXT = 'PAGE_NEXT';
const PAGE_PREV = 'PAGE_PREV';  
const PAGE_CLEAR = 'PAGE_CLEAR';

const pageCount = {
  pageNext() {
    return { type: PAGE_NEXT };
  },
  pagePrev() {
    return { type: PAGE_PREV };
  },
  pageClear() {
    return { type: PAGE_CLEAR}
  }
}
const IS_LAST_PAGE = 'IS_LAST_PAGE';
const NOT_LAST_PAGE = 'NOT_LAST_PAGE';

const isLastPage = () => ({
  type: IS_LAST_PAGE
});

const notLastPage = () => ({
  type: NOT_LAST_PAGE
})

const lastPage = (state = false, action) => {
  switch(action.type) {
    case IS_LAST_PAGE:
      return true;
    case NOT_LAST_PAGE:
      return false;
    default:
      return state;
  }
}
const pageIndex = (state = 0, action) => {
  switch(action.type) {
    case PAGE_NEXT:
      return state + 1;

    case PAGE_PREV:
      return state - 1;

    case PAGE_CLEAR:
      return 0;

    default:
      return state;
  }
}

export { pageCount, lastPage, pageIndex, isLastPage, notLastPage} ;