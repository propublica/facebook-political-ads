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

export { pageCount, PAGE_NEXT, PAGE_PREV, PAGE_CLEAR, IS_LAST_PAGE, NOT_LAST_PAGE, isLastPage, notLastPage} ;