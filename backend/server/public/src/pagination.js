const PAGE_NEXT = 'page_next';
const PAGE_PREV = 'page_prev';
const PAGE_CLEAR = 'page_clear';

const pageCount = {
  pageNext() {
    return { type: PAGE_NEXT };
  },
  pagePrev() {
    return { type: PAGE_PREV };
  },
  pageClear() {
    return { type: PAGE_CLEAR };
  }
};
const IS_LAST_PAGE = 'is_last_page';
const NOT_LAST_PAGE = 'not_last_page';

const isLastPage = () => ({
  type: IS_LAST_PAGE
});

const notLastPage = () => ({
  type: NOT_LAST_PAGE
});

const lastPage = (state = false, action) => {
  switch(action.type) {
  case IS_LAST_PAGE:
    return true;
  case NOT_LAST_PAGE:
    return false;
  default:
    return state;
  }
};

const pageIndex = (state = 0, action) => {
  switch(action.type) {
  case PAGE_NEXT:
    return state + 1;
  case PAGE_PREV:
    if ((state - 1) > 0) {
      return state - 1;
    } else {
      return 0;
    }
  case PAGE_CLEAR:
    return 0;
  default:
    return state;
  }
};

export { pageCount, lastPage, pageIndex, isLastPage, notLastPage} ;
